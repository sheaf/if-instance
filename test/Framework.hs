
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Framework
  ( Test(..), runTest, runTests )
  where

-- base
import Data.Maybe
  ( mapMaybe )
import System.Exit
  ( exitFailure, exitSuccess )

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

runTests :: [ Test ] -> IO ()
runTests tests = do
  let
    results :: [ String ]
    results = mapMaybe runTest tests
  case results of
    [] -> exitSuccess
    _  -> putStrLn ( unlines results ) *> exitFailure
