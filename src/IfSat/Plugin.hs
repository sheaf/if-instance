{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module IfSat.Plugin
  ( plugin )
  where

-- base
import Data.Maybe
  ( catMaybes )
#if !MIN_VERSION_ghc(9,2,0)
import Unsafe.Coerce
  ( unsafeCoerce )
#endif

-- ghc
import GHC.Plugins
  ( Plugin(..)
  , defaultPlugin, purePlugin
  )
import GHC.Data.Bag
  ( unitBag )
import GHC.Tc.Solver.Interact
  ( solveSimpleGivens, solveSimpleWanteds )
import GHC.Tc.Solver.Monad
  ( TcS
  , getTcEvBindsMap, readTcRef, runTcS, runTcSWithEvBinds, traceTcS
#if MIN_VERSION_ghc(9,2,0)
  , wrapTcS
#endif
  )
import GHC.Tc.Types
  ( TcM )
import GHC.Tc.Types.Constraint
  ( isEmptyWC )
import GHC.Utils.Outputable
  ( (<+>), ($$), empty, text, vcat )

-- ghc-tcplugin-api
import GHC.TcPlugin.API
import GHC.TcPlugin.API.Internal
  ( unsafeLiftTcM )

--------------------------------------------------------------------------------
-- Plugin definition.

-- | A type-checking plugin that solves @ct_l || ct_r@ constraints.
-- This allows users to branch on whether @ct_l@ is satisfied.
--
-- To use this plugin, add @{-# OPTIONS_GHC -fplugin=IfSat.Plugin #-}@
-- to your module header.
--
-- A @ct_l || ct_r@  instance is solved by trying to solve @ct_l@:
--
--   - if solving succeeds, the 'Data.Constraint.If.dispatch' function will
--     pick the first branch,
--   - otherwise, 'Data.Constraint.If.dispatch' will pick the second branch.
--
-- This means that the branch selection occurs precisely at the moment
-- at which we solve the @ct_l || ct_r@  constraint.
-- See the documentation of 'Data.Constraint.If.dispatch' for more information.
plugin :: Plugin
plugin =
  defaultPlugin
    { tcPlugin        = \ _args -> Just $ mkTcPlugin ifSatTcPlugin
    , pluginRecompile = purePlugin
    }

ifSatTcPlugin :: TcPlugin
ifSatTcPlugin =
  TcPlugin
    { tcPluginInit    = initPlugin
    , tcPluginSolve   = solver
    , tcPluginRewrite = rewriter
    , tcPluginStop    = \ _ -> pure ()
    }

--------------------------------------------------------------------------------
-- Plugin initialisation.

data PluginDefs
  = PluginDefs
    { orClass    :: !Class
    , isSatTyCon :: !TyCon
    }

findModule :: MonadTcPlugin m => Maybe String -> String -> m Module
findModule mb_pkg modName = do
  findResult <- findImportedModule ( mkModuleName modName ) ( fmap fsLit mb_pkg )
  case findResult of
    Found _ res     -> pure res
    FoundMultiple _ -> error $ "IfSat plugin: found multiple modules named " <> modName <> "."
    _               -> error $ "IfSat plugin: could not find any module named " <> modName <> "."

initPlugin :: TcPluginM Init PluginDefs
initPlugin = do
  ifSatModule <- findModule Nothing "Data.Constraint.If"
  orClass     <- tcLookupClass =<< lookupOrig ifSatModule ( mkClsOcc "||"    )
  isSatTyCon  <- tcLookupTyCon =<< lookupOrig ifSatModule ( mkTcOcc  "IsSat" )
  pure $ PluginDefs { orClass, isSatTyCon }

--------------------------------------------------------------------------------
-- Constraint solving.

solver :: PluginDefs -> [ Ct ] -> [ Ct ] -> TcPluginM Solve TcPluginSolveResult
solver defs givens wanteds
  | null wanteds
  = pure $ TcPluginOk [] []
  | otherwise
  = do
      tcPluginTrace "IfSat solver {" (ppr givens $$ ppr wanteds)
      solveds <- catMaybes <$> traverse ( solveWanted defs givens ) wanteds
      tcPluginTrace "IfSat solver }" empty
      pure $ TcPluginOk solveds []

solveWanted :: PluginDefs -> [ Ct ] -> Ct -> TcPluginM Solve ( Maybe ( EvTerm, Ct ) )
solveWanted defs@( PluginDefs { orClass } ) givens wanted
  | ClassPred cls [ct_l_ty, ct_r_ty] <- classifyPredType ( ctPred wanted )
  , cls == orClass
  = do
    tcPluginTrace "IfSat solver: found (||) constraint"
      ( ppr ct_l_ty $$ ppr ct_r_ty $$ ppr wanted )
    ct_l_ev <- newWanted ( ctLoc wanted ) ct_l_ty
    ct_r_ev <- newWanted ( ctLoc wanted ) ct_r_ty
    let
      ct_l, ct_r :: Ct
      ct_l = mkNonCanonical ct_l_ev
      ct_r = mkNonCanonical ct_r_ev
      ct_l_ev_dest, ct_r_ev_dest :: TcEvDest
      ct_l_ev_dest = ctev_dest ct_l_ev
      ct_r_ev_dest = ctev_dest ct_r_ev
    evBindsVar <- askEvBinds
    -- Start a new Solver run.
    unsafeLiftTcM $ runTcSWithEvBinds evBindsVar $ do
      -- Add back all the Givens.
      traceTcS "IfSat solver: adding Givens to the inert set" (ppr givens)
      solveSimpleGivens givens
      -- Try to solve 'ct_l', using both Givens and top-level instances.
      _ <- solveSimpleWanteds ( unitBag ct_l )
      -- Now look up whether GHC has managed to produce evidence for 'ct_l'.
      mb_ct_l_evTerm <- lookupEvTerm evBindsVar ct_l_ev_dest
      mb_wanted_evTerm <- case mb_ct_l_evTerm of
        Just ( EvExpr ct_l_evExpr ) -> do
          -- We've managed to solve 'ct_l': use the evidence and take the 'True' branch.
          traceTcS "IfSat solver: LHS constraint could be solved"
            ( vcat
              [ text "ct_l =" <+> ppr ct_l_ty
              , text "ev   =" <+> ppr ct_l_evExpr
              ]
            )
          wrapTcS $ ( Just <$> dispatchTrueEvTerm defs ct_l_ty ct_r_ty ct_l_evExpr )
        _ -> do
          -- We couldn't solve 'ct_l': this means we must solve 'ct_r',
          -- to provide evidence needed for the 'False' branch.
          traceTcS "IfSat solver: LHS constraint could not be solved"
            ( text "ct_l =" <+> ppr ct_l_ty )
          -- Try to solve 'ct_r', using both Givens and top-level instances.
          _ <- solveSimpleWanteds ( unitBag ct_r )
          mb_ct_r_evTerm <- lookupEvTerm evBindsVar ct_r_ev_dest
          case mb_ct_r_evTerm of
            Just ( EvExpr ct_r_evExpr ) -> do
              -- We've managed to solve 'ct_r': use the evidence and take the 'False' branch.
              traceTcS "IfSat solver: RHS constraint could be solved"
                ( vcat
                  [ text "ct_r =" <+> ppr ct_r_ty
                  , text "ev   =" <+> ppr ct_r_evExpr
                  ]
                )
              wrapTcS $ ( Just <$> dispatchFalseEvTerm defs ct_l_ty ct_r_ty ct_r_evExpr )
            _ -> do
              -- We could solve neither 'ct_l' not 'ct_r'.
              -- This means we can't solve the disjunction constraint.
              traceTcS "IfSat solver: RHS constraint could not be solved"
                ( text "ct_r =" <+> ppr ct_r_ty )
              pure Nothing
      pure $ ( , wanted ) <$> mb_wanted_evTerm
  | otherwise
  = pure Nothing

-- | Look up whether a 'TcEvDest' has been filled with evidence.
lookupEvTerm :: EvBindsVar -> TcEvDest -> TcS ( Maybe EvTerm )
lookupEvTerm _ ( HoleDest ( CoercionHole { ch_ref = ref } ) ) = do
  mb_co <- readTcRef ref
  traceTcS "IfSat solver: coercion hole" ( ppr mb_co )
  case mb_co of
    Nothing -> pure Nothing
    Just co -> pure . Just $ evCoercion co
lookupEvTerm evBindsVar ( EvVarDest ev_var ) = do
  evBindsMap <- getTcEvBindsMap evBindsVar
  let
    mb_evBind :: Maybe EvBind
    mb_evBind = lookupEvBind evBindsMap ev_var
  traceTcS "IfSat solver: evidence binding" ( ppr mb_evBind )
  case mb_evBind of
    Nothing      -> pure Nothing
    Just ev_bind -> pure . Just $ eb_rhs ev_bind

-- Evidence term for @ct_l || ct_r@ when @ct_l@ is satisfied.
--
-- dispatch =
--   \ @r
--     ( a :: ( IsSat ct_l ~ True, ct_l ) => r )
--     ( _ :: ( IsSat ct_l ~ False, IsSat ct_r ~ True, ct_r ) => r )
--   -> a ct_l_isSat_co ct_l_evTerm
dispatchTrueEvTerm :: PluginDefs -> Type -> Type -> EvExpr -> TcM EvTerm
dispatchTrueEvTerm defs@( PluginDefs { orClass } ) ct_l_ty ct_r_ty ct_l_evTerm = do
  r_name <- newName ( mkTyVarOcc "r" )
  a_name <- newName ( mkVarOcc   "a" )
  let
    r, a, b :: CoreBndr
    r = mkTyVar r_name liftedTypeKind
    a = mkLocalId a_name Many
        ( mkInvisFunTysMany [ sat_eqTy defs ct_l_ty True, ct_l_ty ] r_ty )
    b = mkWildValBinder  Many
        ( mkInvisFunTysMany [ sat_eqTy defs ct_l_ty False, sat_eqTy defs ct_r_ty True, ct_r_ty ] r_ty )
    r_ty :: Type
    r_ty = mkTyVarTy r
  pure . EvExpr $
    mkCoreConApps ( classDataCon orClass )
      [ Type ct_l_ty
      , Type ct_r_ty
      , mkCoreLams [ r, a, b ]
        ( mkCoreApps ( Var a )
          [ sat_co_expr defs ct_l_ty True
          , ct_l_evTerm
          ]
        )
      ]

-- Evidence term for @ct_l || ct_r@ when @ct_l@ isn't satisfied, but @ct_r@ is.
--
-- dispatch =
--   \ @r
--     ( _ :: ( IsSat ct_l ~ True, ct_l ) => r )
--     ( b :: ( IsSat ct_l ~ False, IsSat ct_r ~ True, ct_r ) => r )
--   -> b ct_l_notSat_co ct_r_isSat_co ct_r_evTerm
dispatchFalseEvTerm :: PluginDefs -> Type -> Type -> EvExpr -> TcM EvTerm
dispatchFalseEvTerm defs@( PluginDefs { orClass } ) ct_l_ty ct_r_ty ct_r_evExpr = do
  r_name <- newName ( mkTyVarOcc "r" )
  b_name <- newName ( mkVarOcc   "b" )
  let
    r, a, b :: CoreBndr
    r = mkTyVar r_name liftedTypeKind
    a = mkWildValBinder  Many
        ( mkInvisFunTysMany [ sat_eqTy defs ct_l_ty True, ct_l_ty ] r_ty )
    b = mkLocalId b_name Many
        ( mkInvisFunTysMany [ sat_eqTy defs ct_l_ty False, sat_eqTy defs ct_r_ty True, ct_r_ty ] r_ty )
    r_ty :: Type
    r_ty = mkTyVarTy r
  pure . EvExpr $
    mkCoreConApps ( classDataCon orClass )
      [ Type ct_l_ty
      , Type ct_r_ty
      , mkCoreLams [ r, a, b ]
        ( mkCoreApps ( Var b )
          [ sat_co_expr defs ct_l_ty False
          , sat_co_expr defs ct_r_ty True
          , ct_r_evExpr
          ]
        )
      ]

-- @ sat_eqTy defs ct_ty b @ represents the type @ IsSat ct ~ b @.
sat_eqTy :: PluginDefs -> Type -> Bool -> Type
sat_eqTy ( PluginDefs { isSatTyCon } ) ct_ty booly
  = mkTyConApp eqTyCon
      [ boolTy, mkTyConApp isSatTyCon [ct_ty], rhs ]
  where
    rhs :: Type
    rhs = if booly then tru else fls

-- @ sat_co_expr defs ct_ty b @ is an expression of type @ IsSat ct ~ b @.
sat_co_expr :: PluginDefs -> Type -> Bool -> EvExpr
sat_co_expr ( PluginDefs { isSatTyCon } ) ct_ty booly
  = mkCoreConApps eqDataCon
      [ Type boolTy
      , Type $ mkTyConApp isSatTyCon [ ct_ty ]
      , Type rhs
      , Coercion $
          mkPluginUnivCo ( "IfSat :" <> show booly )
          Nominal
          ( mkTyConApp isSatTyCon [ct_ty] ) rhs
      ]
  where
    rhs :: Type
    rhs = if booly then tru else fls

fls, tru :: Type
fls = mkTyConTy promotedFalseDataCon
tru = mkTyConTy promotedTrueDataCon

--------------------------------------------------------------------------------

rewriter :: PluginDefs -> UniqFM TyCon TcPluginRewriter
rewriter defs@( PluginDefs { isSatTyCon } )
  = listToUFM [ ( isSatTyCon, isSatRewriter defs ) ]

isSatRewriter :: PluginDefs -> [Ct] -> [Type] -> TcPluginM Rewrite TcPluginRewriteResult
isSatRewriter ( PluginDefs { isSatTyCon } ) givens [ct_ty] = do
  tcPluginTrace "IfSat rewriter {" (ppr givens $$ ppr ct_ty)
  rewriteEnv <- askRewriteEnv
  ct_ev <- newWanted ( rewriteEnvCtLoc rewriteEnv ) ct_ty
  let
    ct :: Ct
    ct = mkNonCanonical ct_ev
  -- Start a new Solver run.
  ( redn, _ ) <- unsafeLiftTcM $ runTcS $ do
    -- Add back all the Givens.
    traceTcS "IfSat rewriter: adding Givens to the inert set" (ppr givens)
    solveSimpleGivens givens
    -- Try to solve 'ct', using both Givens and top-level instances.
    residual_wc <- solveSimpleWanteds ( unitBag ct )
    -- When there are residual Wanteds, we couldn't solve the constraint.
    let
      is_sat :: Bool
      is_sat = isEmptyWC residual_wc
      sat :: Type
      sat
        | is_sat
        = mkTyConTy promotedTrueDataCon
        | otherwise
        = mkTyConTy promotedFalseDataCon
    pure $ mkTyFamAppReduction ( "IsSat: " <> show is_sat ) Nominal isSatTyCon [ct_ty] sat
  tcPluginTrace "IfSat rewriter }" ( ppr redn )
  pure $ TcPluginRewriteTo redn []
isSatRewriter _ _ _ = pure TcPluginNoRewrite

--------------------------------------------------------------------------------

#if !MIN_VERSION_ghc(9,2,0)
wrapTcS :: TcM a -> TcS a
wrapTcS = unsafeCoerce const
#endif
