module Main where

import Kabigon.Prelude

import Prelude (print)

import Kabigon.Parser

main :: IO ()
main = print =<< parseSavefile "test.sav"
