
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
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
#if MIN_VERSION_ghc(9,4,0)
import GHC.Exts
  ( withDict )
#endif

-- IfSat
import Data.Constraint.If
  ( IfSat(ifSat) )

--------------------------------------------------------------------------------

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

-- No "MyShow ( Int -> Int -> Int )" instance.
test2 :: String
test2 = myShowAnything ( (+) :: Int -> Int -> Int )

data A = A

myShowA :: IfSat ( MyShow A ) => String
myShowA = myShowAnything A

#if MIN_VERSION_ghc(9,4,0)
-- Should use the instance locally provided by "withDict".
test3 :: String
test3 =
  withDict @( A -> String ) @( MyShow A )
    ( \ _ -> "A" )
    myShowA
#endif

-- No "MyShow A" instance.
test4 :: String
test4 = myShowA

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
  [ Test "test1" test1 "123"
  , Test "test2" test2 "<<no MyShow instance>>"
#if MIN_VERSION_ghc(9,4,0)
  , Test "test3" test3 "A"
#endif
  , Test "test4" test4 "<<no MyShow instance>>"
  ]

main :: IO ()
main = do
  let
    results :: [ String ]
    results = mapMaybe runTest tests
  case results of
    [] -> exitSuccess
    _  -> putStrLn ( unlines results ) *> exitFailure
