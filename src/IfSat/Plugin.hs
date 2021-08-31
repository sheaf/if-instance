{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

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
  ( getTcEvBindsMap, readTcRef, runTcS, runTcSWithEvBinds, traceTcS
#if MIN_VERSION_ghc(9,2,0)
  , wrapTcS
#else
  , TcS
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

-- | A type-checking plugin that solves @MyCt ct@ constraints.
-- This allows users to branch on whether @ct@ is satisfied.
--
-- To use this plugin, add @{-# OPTIONS_GHC -fplugin=IfSat.Plugin #-}@
-- to your module header.
--
-- A @MyCt ct@ instance is solved by trying to solve @ct@:
--
--   - if solving succeeds, the 'Data.Constraint.If.ifSat' function will
--     pick the first branch,
--   - otherwise, 'Data.Constraint.If.ifSat' will pick the second branch.
--
-- This means that the branch selection occurs precisely at the moment
-- at which we solve the @IfSat ct@ constraint.
-- See the documentation of 'Data.Constraint.If.IfSat' for more information.
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
    { ifSatClass  :: !Class
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
  ifSatClass  <- tcLookupClass =<< lookupOrig ifSatModule ( mkClsOcc "IfSat" )
  isSatTyCon  <- tcLookupTyCon =<< lookupOrig ifSatModule ( mkTcOcc  "IsSat" )
  pure $ PluginDefs { ifSatClass, isSatTyCon }

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
solveWanted defs@( PluginDefs { ifSatClass } ) givens wanted
  | ClassPred cls [ct_ty] <- classifyPredType ( ctPred wanted )
  , cls == ifSatClass
  = do
    tcPluginTrace "IfSat solver: found IfSat constraint" ( ppr wanted )
    ct_ev <- newWanted ( ctLoc wanted ) ct_ty
    let
      ct :: Ct
      ct = mkNonCanonical ct_ev
      ct_ev_dest :: TcEvDest
      ct_ev_dest = ctev_dest ct_ev
    evBindsVar <- askEvBinds
    -- Start a new Solver run.
    unsafeLiftTcM $ runTcSWithEvBinds evBindsVar $ do
      -- Add back all the Givens.
      traceTcS "IfSat solver: adding Givens to the inert set" (ppr givens)
      solveSimpleGivens givens
      -- Try to solve 'ct', using both Givens and top-level instances.
      _ <- solveSimpleWanteds ( unitBag ct )
      -- Now look up whether GHC has managed to produce evidence for 'ct'.
      mb_ct_evTerm <-
        case ct_ev_dest of
          HoleDest ( CoercionHole { ch_ref = ref } ) -> do
            mb_co <- readTcRef ref
            traceTcS "IfSat solver: coercion hole" (ppr mb_co)
            case mb_co of
              Nothing -> pure Nothing
              Just co -> pure . Just $ evCoercion co
          EvVarDest ev_var -> do
            evBindsMap <- getTcEvBindsMap evBindsVar
            let
              mb_evBind :: Maybe EvBind
              mb_evBind = lookupEvBind evBindsMap ev_var
            traceTcS "IfSat solver: evidence binding" (ppr mb_evBind)
            case mb_evBind of
              Nothing      -> pure Nothing
              Just ev_bind -> pure . Just $ eb_rhs ev_bind
      wanted_evTerm <- case mb_ct_evTerm of
        Just ( EvExpr ct_evExpr ) -> do
          -- We've managed to solve 'ct': use the evidence and take the 'True' branch.
          traceTcS "IfSat solver: constraint could be solved"
            ( vcat
              [ text "ct =" <+> ppr ct_ty
              , text "ev =" <+> ppr ct_evExpr
              ]
            )
          wrapTcS $ ifSatTrueEvTerm defs ct_ty ct_evExpr
        _ -> do
          -- We couldn't solve 'ct': take the 'False' branch.
          traceTcS "IfSat solver: constraint could not be solved"
            ( text "ct =" <+> ppr ct_ty )
          wrapTcS $ ifSatFalseEvTerm defs ct_ty
      pure $ Just ( wanted_evTerm, wanted )
  | otherwise
  = pure Nothing

-- Evidence term for @IfSat ct@ when @ct@ isn't satisfied.
-- IfSat = \ @r (a :: ( IsSat ct ~ True, ct ) => r) (_ :: IsSat ct ~ False => r) -> a isSat_co ct_evTerm
ifSatTrueEvTerm :: PluginDefs -> Type -> EvExpr -> TcM EvTerm
ifSatTrueEvTerm defs@( PluginDefs { ifSatClass } ) ct_ty ct_evTerm = do
  r_name <- newName ( mkTyVarOcc "r" )
  a_name <- newName ( mkVarOcc   "a" )
  let
    r, a, b :: CoreBndr
    r = mkTyVar r_name liftedTypeKind
    a = mkLocalId a_name Many ( mkInvisFunTyMany (sat_eqTy defs ct_ty True ) $ mkInvisFunTyMany ct_ty r_ty )
    b = mkWildValBinder  Many ( mkInvisFunTyMany (sat_eqTy defs ct_ty False) r_ty )
    r_ty :: Type
    r_ty = mkTyVarTy r
  pure . EvExpr $
    mkCoreConApps ( classDataCon ifSatClass )
      [ Type ct_ty
      , mkCoreLams [ r, a, b ]
        ( mkCoreApps ( Var a ) [ sat_co_expr defs ct_ty True, ct_evTerm ] )
      ]

-- Evidence term for @IfSat ct@ when @ct@ isn't satisfied.
-- IfSat = \ @r (_ :: ( IsSat ct ~ True, ct ) => r) (b :: IsSat ct ~ False => r) -> b notSat_co
ifSatFalseEvTerm :: PluginDefs -> Type -> TcM EvTerm
ifSatFalseEvTerm defs@( PluginDefs { ifSatClass } ) ct_ty = do
  r_name <- newName ( mkTyVarOcc "r" )
  b_name <- newName ( mkVarOcc   "b" )
  let
    r, a, b :: CoreBndr
    r = mkTyVar r_name liftedTypeKind
    a = mkWildValBinder  Many ( mkInvisFunTyMany (sat_eqTy defs ct_ty True ) $ mkInvisFunTyMany ct_ty r_ty )
    b = mkLocalId b_name Many ( mkInvisFunTyMany (sat_eqTy defs ct_ty False) r_ty )
    r_ty :: Type
    r_ty = mkTyVarTy r
  pure . EvExpr $
    mkCoreConApps ( classDataCon ifSatClass )
      [ Type ct_ty
      , mkCoreLams [ r, a, b ]
        ( mkCoreApps ( Var b ) [ sat_co_expr defs ct_ty False ] )
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
