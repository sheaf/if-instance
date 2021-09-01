
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin=IfSat.Plugin #-}

module M1 where

-- base
import Data.Kind
  ( Type )

-- if-instance
import Data.Constraint.If
  ( IfSat, ifSat, IsSat )

--------------------------------------------------------------------------------

showFun :: forall (a :: Type). IfSat ( Show ( a -> a ) ) => ( a -> a ) -> String
showFun = ifSat @( Show (a -> a) ) show ( \ _ -> "<<function>>" )

test1 :: ( Bool -> Bool ) -> String
test1 fun = showFun fun

class C

type F :: Bool -> Type
type family F b where
  F False = Float
  F True  = String

foo :: Float -> F (IsSat C)
foo x = x
