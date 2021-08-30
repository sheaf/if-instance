
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=IfCt.Plugin #-}

module M1 where

-- base
import Data.Kind
  ( Type )

-- if-instance
import Data.Constraint.If
  ( IfCt(ifCt) )

--------------------------------------------------------------------------------

showFun :: forall (a :: Type). IfCt ( Show ( a -> a ) ) => ( a -> a ) -> String
showFun = ifCt @( Show (a -> a) ) show ( \ _ -> "<<function>>" )

test1 :: ( Bool -> Bool ) -> String
test1 fun = showFun fun
