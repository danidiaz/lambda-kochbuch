{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Arrows #-}

module Main where

import Prelude hiding ((.),id)
import Data.Monoid
import Data.List.NonEmpty

import Control.Category
import Control.Arrow
import Control.Monad

import Test.Tasty
import Test.Tasty.HUnit (testCase,assertEqual,assertBool)

import Lang.Lambda.Simple

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
                          ]

