module Main where

import Test.DocTest

main :: IO ()
main = doctest ["lib/Lang/Lambda.hs"
               ,"lib/Lang/Lambda/Types.hs"
               ,"lib/Lang/Lambda/Parser.hs"]
