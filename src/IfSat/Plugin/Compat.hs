{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module IfSat.Plugin.Compat
  ( wrapTcS, getRestoreTcS )
  where

-- base
import Unsafe.Coerce
  ( unsafeCoerce )

-- ghc
#if MIN_VERSION_ghc(9,4,0)
import GHC.Tc.Solver.InertSet
  ( WorkList, InertSet )
#endif
import GHC.Tc.Solver.Monad
  ( TcS
#if MIN_VERSION_ghc(9,1,0)
  , TcLevel, wrapTcS
#endif
#if !MIN_VERSION_ghc(9,4,0)
  , WorkList, InertSet
#endif
  )
import GHC.Tc.Types
  ( TcM, TcRef )
import GHC.Tc.Types.Evidence
  ( EvBindsVar(..) )

-- ghc-tcplugin-api
import GHC.TcPlugin.API
  ( readTcRef, writeTcRef )

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
