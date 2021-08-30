{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module IfCt.Plugin
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
  ( getTcEvBindsMap, readTcRef, runTcSWithEvBinds, traceTcS
#if MIN_VERSION_ghc(9,2,0)
  , wrapTcS
#else
  , TcS
#endif
  )
import GHC.Tc.Types
  ( TcM )
import GHC.Utils.Outputable
  ( (<+>), ($$), empty, text, vcat )

-- ghc-tcplugin-api
import GHC.TcPlugin.API
import GHC.TcPlugin.API.Internal
  ( unsafeLiftTcM )

--------------------------------------------------------------------------------
-- Plugin definition.

-- | A type-checking plugin that solves @MyCt ct@ constraints.
-- Theis allows users to branch on whether @ct@ is satisfied.
--
-- To use this plugin, add @{-# OPTIONS_GHC -fplugin=IfCt.Plugin #-}@
-- to your module header.
--
-- A @MyCt ct@ instance is solved by trying to solve @ct@:
--
--   - if solving succeeds, the 'Data.Constraint.If.ifCt' function will
--     pick the first branch,
--   - otherwise, 'Data.Constraint.If.ifCt' will pick the second branch.
--
-- This means that the branch selection occurs precisely at the moment
-- at which we solve the @IfCt ct@ constraint.
-- See the documentation of 'Data.Constraint.If.IfCt' for more information.
plugin :: Plugin
plugin =
  defaultPlugin
    { tcPlugin        = \ _args -> Just $ mkTcPlugin ifCtTcPlugin
    , pluginRecompile = purePlugin
    }

ifCtTcPlugin :: TcPlugin
ifCtTcPlugin =
  TcPlugin
    { tcPluginInit    = initPlugin
    , tcPluginSolve   = solver
    , tcPluginRewrite = \ _ -> emptyUFM
    , tcPluginStop    = \ _ -> pure ()
    }

--------------------------------------------------------------------------------
-- Plugin initialisation.

data PluginDefs
  = PluginDefs
    { ifCtClass :: !Class }

findModule :: MonadTcPlugin m => Maybe String -> String -> m Module
findModule mb_pkg modName = do
  findResult <- findImportedModule ( mkModuleName modName ) ( fmap fsLit mb_pkg )
  case findResult of
    Found _ res     -> pure res
    FoundMultiple _ -> error $ "IfCt plugin: found multiple modules named " <> modName <> "."
    _               -> error $ "IfCt plugin: could not find any module named " <> modName <> "."

initPlugin :: TcPluginM Init PluginDefs
initPlugin = do
  ifCtModule <- findModule Nothing "Data.Constraint.If"
  ifCtClass  <- tcLookupClass =<< lookupOrig ifCtModule ( mkClsOcc "IfCt" )
  pure $ PluginDefs { ifCtClass }

--------------------------------------------------------------------------------
-- Constraint solving.

solver :: PluginDefs -> [ Ct ] -> [ Ct ] -> TcPluginM Solve TcPluginSolveResult
solver defs givens wanteds
  | null wanteds
  = pure $ TcPluginOk [] []
  | otherwise
  = do
      tcPluginTrace "IfCt plugin {" (ppr givens $$ ppr wanteds)
      solveds <- catMaybes <$> traverse ( solveWanted defs givens ) wanteds
      tcPluginTrace "IfCt plugin }" empty
      pure $ TcPluginOk solveds []

solveWanted :: PluginDefs -> [ Ct ] -> Ct -> TcPluginM Solve ( Maybe ( EvTerm, Ct ) )
solveWanted defs@( PluginDefs { ifCtClass } ) givens wanted
  | ClassPred cls [ct_ty] <- classifyPredType ( ctPred wanted )
  , cls == ifCtClass
  = do
    tcPluginTrace "IfCt plugin: found IfCt constraint" ( ppr wanted )
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
      traceTcS "IfCt plugin: adding Givens to the inert set" (ppr givens)
      solveSimpleGivens givens
      -- Try to solve 'ct', using both Givens and top-level instances.
      _ <- solveSimpleWanteds ( unitBag ct )
      -- Now look up whether GHC has managed to produce evidence for 'ct'.
      mb_ct_evTerm <-
        case ct_ev_dest of
          HoleDest ( CoercionHole { ch_ref = ref } ) -> do
            mb_co <- readTcRef ref
            traceTcS "IfCt plugin: coercion hole" (ppr mb_co)
            case mb_co of
              Nothing -> pure Nothing
              Just co -> pure . Just $ evCoercion co
          EvVarDest ev_var -> do
            evBindsMap <- getTcEvBindsMap evBindsVar
            let
              mb_evBind :: Maybe EvBind
              mb_evBind = lookupEvBind evBindsMap ev_var
            traceTcS "IfCt plugin: evidence binding" (ppr mb_evBind)
            case mb_evBind of
              Nothing      -> pure Nothing
              Just ev_bind -> pure . Just $ eb_rhs ev_bind
      wanted_evTerm <- case mb_ct_evTerm of
        Just ( EvExpr ct_evExpr ) -> do
          -- We've managed to solve 'ct': use the evidence and take the 'True' branch.
          traceTcS "IfCt plugin: constraint could be solved"
            ( vcat
              [ text "ct =" <+> ppr ct_ty
              , text "ev =" <+> ppr ct_evExpr
              ]
            )
          wrapTcS $ ifCtTrueEvTerm defs ct_ty ct_evExpr
        _ -> do
          -- We couldn't solve 'ct': take the 'False' branch.
          traceTcS "IfCt plugin: constraint could not be solved"
            ( text "ct =" <+> ppr ct_ty )
          wrapTcS $ ifCtFalseEvTerm defs ct_ty
      pure $ Just ( wanted_evTerm, wanted )
  | otherwise
  = pure Nothing

-- Evidence term for @IfCt ct@ when @ct@ isn't satisfied.
-- ifCt = \ @r (a :: ct => r) (_ :: r) -> a ct_evTerm
ifCtTrueEvTerm :: PluginDefs -> Type -> EvExpr -> TcM EvTerm
ifCtTrueEvTerm ( PluginDefs { ifCtClass } ) ct_ty ct_evTerm = do
  r_name <- newName ( mkTyVarOcc "r" )
  a_name <- newName ( mkVarOcc   "a" )
  let
    r, a, b :: CoreBndr
    r = mkTyVar r_name liftedTypeKind
    a = mkLocalId a_name Many ( mkInvisFunTyMany ct_ty r_ty )
    b = mkWildValBinder Many r_ty
    r_ty :: Type
    r_ty = mkTyVarTy r
  pure . EvExpr $
    mkCoreConApps ( classDataCon ifCtClass )
      [ Type ct_ty
      , mkCoreLams [ r, a, b ]
        ( mkCoreApps ( Var a ) [ ct_evTerm ] )
      ]

-- Evidence term for @IfCt ct@ when @ct@ isn't satisfied.
-- ifCt = \ @r (_ :: ct => r) (b :: r) -> b
ifCtFalseEvTerm :: PluginDefs -> Type -> TcM EvTerm
ifCtFalseEvTerm ( PluginDefs { ifCtClass } ) ct_ty = do
  r_name <- newName ( mkTyVarOcc "r" )
  b_name <- newName ( mkVarOcc   "b" )
  let
    r, a, b :: CoreBndr
    r = mkTyVar r_name liftedTypeKind
    a = mkWildValBinder Many ( mkInvisFunTyMany ct_ty r_ty )
    b = mkLocalId b_name Many r_ty
    r_ty :: Type
    r_ty = mkTyVarTy r
  pure . EvExpr $
    mkCoreConApps ( classDataCon ifCtClass )
      [ Type ct_ty
      , mkCoreLams [ r, a, b ] ( Var b )
      ]

--------------------------------------------------------------------------------

#if !MIN_VERSION_ghc(9,2,0)
wrapTcS :: TcM a -> TcS a
wrapTcS = unsafeCoerce const
#endif