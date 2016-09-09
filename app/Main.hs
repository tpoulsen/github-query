{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T (Text, intercalate, pack)
import Query

main :: IO ()
main = runQuery "<2012-01-01"
