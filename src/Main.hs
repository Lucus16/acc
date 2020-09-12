{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Text.Megaparsec (errorBundlePretty, parse)
import C (assembly, fdef)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

program :: Text
program = "int main() { return 2; }"

parsed = parse fdef "program" program

main :: IO ()
main = case parse fdef "program" program of
         Left errorBundle -> putStr $ errorBundlePretty errorBundle
         Right ast        -> TextIO.putStr $ Text.unlines $ assembly ast
