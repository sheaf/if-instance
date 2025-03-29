{-# LANGUAGE CPP #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module IfSat.Plugin.Compat
  ( -- * Dealing with TcS state
    wrapTcS, getRestoreTcS
    -- * Dealing with mutable references
  , UnfilledRef(..), unfilledRefsOfType, unfillMutableRef
  )
  where

-- base
import Control.Monad
  ( when )
import Unsafe.Coerce
  ( unsafeCoerce )

-- ghc
import GHC.Core.Coercion
  ( mkCoVarCo, mkHoleCo, coVarName, coHoleCoVar )
import GHC.Core.Type
  ( TyCoMapper(..), mapTyCo )
#if MIN_VERSION_ghc(9,4,0)
import GHC.Tc.Solver.InertSet
  ( WorkList )
#endif
#if MIN_VERSION_ghc(9,1,0)
import GHC.Tc.Solver.Monad
  ( wrapTcS )
#endif
#if !MIN_VERSION_ghc(9,4,0)
import GHC.Tc.Solver.Monad
  ( WorkList )
#endif
import GHC.Tc.Types
  ( TcM, TcRef )

import GHC.Tc.Utils.TcMType
  ( isUnfilledMetaTyVar )
import GHC.Tc.Utils.TcType
  ( MetaDetails(..), metaTyVarRef )
import GHC.Tc.Types.Evidence
  ( EvBindsVar(..) )

-- transformers
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Writer.CPS ( WriterT )
import qualified Control.Monad.Trans.Writer.CPS as Writer

-- ghc-tcplugin-api
import GHC.TcPlugin.API
import GHC.Types.Name.Env
import GHC.Types.Var (tyVarName)

--------------------------------------------------------------------------------

-- | Capture the current 'TcS' state, returning an action which restores
-- the fields of 'TcSEnv' as appropriate after running a test-run
-- of 'solveSimpleWanteds' and deciding to backtrack.
getRestoreTcS :: TcS (TcS ())
getRestoreTcS = do
  shim_tcs_env <- getShimTcSEnv
  let ev_binds_var   = shim_tcs_ev_binds shim_tcs_env
      unif_var       = shim_tcs_unified  shim_tcs_env
#if MIN_VERSION_ghc(9,1,0)
      unif_lvl_var   = shim_tcs_unif_lvl shim_tcs_env
#endif
      unit_count_var = shim_tcs_count    shim_tcs_env
  wrapTcS $ do
    restore_evBinds <- case ev_binds_var of
      EvBindsVar { ebv_binds = ev_binds_ref
                 , ebv_tcvs  = ev_cvs_ref } ->
        do ev_binds <- readTcRef ev_binds_ref
           ev_cvs   <- readTcRef ev_cvs_ref
           return do
             writeTcRef ev_binds_ref ev_binds
             writeTcRef ev_cvs_ref   ev_cvs
      CoEvBindsVar { ebv_tcvs = ev_cvs_ref } ->
        do ev_cvs   <- readTcRef ev_cvs_ref
           return do
             writeTcRef ev_cvs_ref   ev_cvs

    unif         <- readTcRef unif_var
#if MIN_VERSION_ghc(9,1,0)
    unif_lvl     <- readTcRef unif_lvl_var
#endif
    count        <- readTcRef unit_count_var
    return $ wrapTcS $ do
      restore_evBinds
      writeTcRef unif_var       unif
#if MIN_VERSION_ghc(9,1,0)
      writeTcRef unif_lvl_var   unif_lvl
#endif
      writeTcRef unit_count_var count

  -- NB: no need to reset 'tcs_inerts' or 'tcs_worklist', because
  -- 'solveSimpleWanteds' calls 'nestTcS', which appropriately resets
  -- both of those fields.

#if !MIN_VERSION_ghc(9,1,0)
wrapTcS :: TcM a -> TcS a
wrapTcS = unsafeCoerce const
#endif

-- Obtain the 'TcSEnv' underlying the 'TcS' monad (in the form of a 'ShimTcSEnv').
getShimTcSEnv :: TcS ShimTcSEnv
getShimTcSEnv = unsafeCoerce ( return :: ShimTcSEnv -> TcM ShimTcSEnv )

-- | A shim copy of "GHC.Tc.Solver.Monad.TcSEnv", to work around the
-- fact that it isn't exported.
--
-- Needs to be manually kept in sync with 'TcSEnv' to avoid segfaults due
-- to the use of 'unsafeCoerce' in 'getShimTcSEnv'.
data ShimTcSEnv
  = ShimTcSEnv
  { shim_tcs_ev_binds           :: EvBindsVar
  , shim_tcs_unified            :: TcRef Int
#if MIN_VERSION_ghc(9,1,0)
  , shim_tcs_unif_lvl           :: TcRef (Maybe TcLevel)
#endif
  , shim_tcs_count              :: TcRef Int
  , shim_tcs_inerts             :: TcRef InertSet
#if MIN_VERSION_ghc(9,3,0)
  , shim_tcs_abort_on_insoluble :: Bool
#endif
  , shim_tcs_worklist           :: TcRef WorkList
  }

--------------------------------------------------------------------------------

-- | A mutable reference that was originally unfilled
data UnfilledRef
  -- | A metavariable that was originally unfilled
  = UnfilledMeta   !( TcRef MetaDetails )
  -- | A coercion hole that was originally unfilled
  | UnfilledCoHole !( TcRef ( Maybe Coercion ) )

-- | Gather all the unfilled mutable references of a type: unfilled
-- metavariables and unfilled coercion holes.
unfilledRefsOfType :: TcType -> TcM [ UnfilledRef ]
unfilledRefsOfType ty0
  = fmap
#if MIN_VERSION_ghc(9,3,0)
      nonDetNameEnvElts
#else
      nameEnvElts
#endif
  $ Writer.execWriterT
  $ go_ty ty0
  where
    (go_ty, _go_tys, _go_co, _go_cos) =
      mapTyCo @( WriterT ( NameEnv UnfilledRef ) TcM ) $
        -- Use a NameEnv to avoid collecting the same reference twice
        -- (not that it would be particularly harmful to do so).
        TyCoMapper
          { tcm_tyvar = \ _ tv -> do
              unfilled_meta <- lift $ isUnfilledMetaTyVar tv
              when unfilled_meta do
                let nm = tyVarName tv
                    ref = UnfilledMeta $ metaTyVarRef tv
                Writer.tell $ unitNameEnv nm ref
              return $ mkTyVarTy tv
          , tcm_tycobinder =
#if MIN_VERSION_ghc(9,7,0)
              \ () tv _ftf k -> k () tv
#else
              \ () tv _af -> return ( (), tv )
#endif
          , tcm_tycon = return
          , tcm_covar = \ _ cv -> return $ mkCoVarCo cv
          , tcm_hole = \ _ hole@(CoercionHole { ch_ref = hole_ref }) -> do
              hole_contents <- lift $ readTcRef hole_ref
              case hole_contents of
                Nothing -> do
                  let nm = coVarName $ coHoleCoVar hole
                      ref = UnfilledCoHole hole_ref
                  Writer.tell $ unitNameEnv nm ref
                Just {} ->
                  return ()
              return $ mkHoleCo hole
          }

-- | Restore a mutable reference to the unfilled state.
unfillMutableRef :: UnfilledRef -> TcM ()
unfillMutableRef = \case
  UnfilledMeta   ref  -> writeTcRef ref  Flexi
  UnfilledCoHole hole -> writeTcRef hole Nothing
