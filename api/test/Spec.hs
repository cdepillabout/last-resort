{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [subTests]

subTests :: TestTree
subTests = testGroup "sub tests" [footest]

footest :: TestTree
footest = testCase "foo bar" $ (1 :: Int) @?= 1
