
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=IfSat.Plugin #-}
{-# OPTIONS_GHC -dcore-lint #-}

{-# OPTIONS_GHC -fno-specialise #-}

module Tests
  ( test1, test1b
  , test2, test2b
#if MIN_VERSION_ghc(9,3,0)
  , test3, test3b
#endif
  , test4, test4b
  )
  where

-- base
import Data.Kind
  ( Constraint, Type )
#if MIN_VERSION_ghc(9,3,0)
import GHC.Exts
  ( withDict )
#endif

-- IfSat
import Data.Constraint.If
  ( IfSat, ifSat, IsSat )

--------------------------------------------------------------------------------

type BoolI :: Bool -> Constraint
class BoolI b where
  boolI :: Bool
instance BoolI True where
  boolI = True
instance BoolI False where
  boolI = False

type MyShow :: Type -> Constraint
class MyShow a where
  myShow :: a -> String

instance MyShow Int where
  myShow = show

myShowAnything :: forall a. IfSat ( MyShow a ) => a -> String
myShowAnything = ifSat @( MyShow a ) yes no
  where
    yes :: MyShow a => a -> String
    yes = myShow
    no :: a -> String
    no _ = "<<no MyShow instance>>"

-- Should use the "MyShow Int" instance.
test1 :: String
test1 = myShowAnything ( 123 :: Int )

test1b :: Bool
test1b = boolI @( IsSat ( MyShow Int ) )

-- No "MyShow ( Int -> Int -> Int )" instance.
test2 :: String
test2 = myShowAnything ( (+) :: Int -> Int -> Int )

test2b :: Bool
test2b = boolI @( IsSat ( MyShow ( Int -> Int -> Int ) ) )

data A = A

myShowA :: IfSat ( MyShow A ) => String
myShowA = myShowAnything A

#if MIN_VERSION_ghc(9,3,0)
-- Should use the instance locally provided by "withDict".
test3 :: String
test3 =
  withDict @( MyShow A ) @( A -> String )
    ( \ _ -> "A" )
    myShowA

test3b :: Bool
test3b =
  withDict @( MyShow A ) @( A -> String )
    ( \ _ -> "A" )
    ( boolI @( IsSat ( MyShow A ) ) )
#endif

-- No "MyShow A" instance.
test4 :: String
test4 = myShowA

test4b :: Bool
test4b = boolI @( IsSat ( MyShow A ) )
