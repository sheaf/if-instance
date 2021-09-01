
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=IfSat.Plugin #-}

module M1 where

-- base
import Data.Kind
  ( Type )

-- if-instance
import Data.Constraint.If
  ( IfSat, ifSat )

--------------------------------------------------------------------------------

showFun :: forall (a :: Type). IfSat ( Show ( a -> a ) ) => ( a -> a ) -> String
showFun = ifSat @( Show (a -> a) ) show ( \ _ -> "<<function>>" )

test1 :: ( Bool -> Bool ) -> String
test1 fun = showFun fun
