module Main where

import System.Exit (exitFailure)
import Data.Monoid
import Test.Framework 
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Huma

-- Some Tests
tests = TestList [ 
  TestCase $ assertEqual "something" 1 2,
  TestCase $ assertEqual "Games" (show fakeGame) ""
  ]

main :: IO ()
main = defaultMainWithOpts
       [testGroup "Hunit tests" hunitTests] mempty where
       hunitTests = hUnitTestToTests tests
