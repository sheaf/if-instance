
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=IfSat.Plugin #-}
{-# OPTIONS_GHC -dcore-lint #-}

module Main where

-- base
import Data.Maybe
  ( mapMaybe )
import Data.Kind
  ( Constraint, Type )
import System.Exit
  ( exitFailure, exitSuccess )
#if MIN_VERSION_ghc(9,3,0)
import GHC.Exts
  ( withDict )
#endif

-- IfSat
import Data.Constraint.If
  ( IfSat(ifSat), IsSat )

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
  withDict @( A -> String ) @( MyShow A )
    ( \ _ -> "A" )
    myShowA

test3b :: Bool
test3b =
  withDict @( A -> String ) @( MyShow A )
    ( \ _ -> "A" )
    ( boolI @( IsSat ( MyShow A ) ) )
#endif

-- No "MyShow A" instance.
test4 :: String
test4 = myShowA

test4b :: Bool
test4b = boolI @( IsSat ( MyShow A ) )

--------------------------------------------------------------------------------

data Test where
  Test
    :: ( Show a, Eq a )
    => { testName     :: String
       , testActual   :: a
       , testExpected :: a
       }
    -> Test

runTest :: Test -> Maybe String
runTest ( Test { testName, testActual, testExpected } )
  | testActual == testExpected
  = Nothing
  | otherwise
  = Just $
      "\n" <>
      "Test '" <> testName <> "' failed.\n" <>
      "Expected: " <> show testExpected <> "\n" <>
      "  Actual: " <> show testActual

tests :: [ Test ]
tests =
  [ Test "test1"  test1  "123"
  , Test "test1b" test1b True
  , Test "test2"  test2  "<<no MyShow instance>>"
  , Test "test2b" test2b False
#if MIN_VERSION_ghc(9,3,0)
  , Test "test3"  test3  "A"
  , Test "test3b" test3b True
#endif
  , Test "test4"  test4  "<<no MyShow instance>>"
  , Test "test4b" test4b False
  ]

main :: IO ()
main = do
  let
    results :: [ String ]
    results = mapMaybe runTest tests
  case results of
    [] -> exitSuccess
    _  -> putStrLn ( unlines results ) *> exitFailure
