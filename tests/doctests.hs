module Main where

import Test.DocTest

main :: IO ()
main = doctest ["lib/Lang/Lambda/Simple.hs"]
