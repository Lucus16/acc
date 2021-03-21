{-# LANGUAGE OverloadedStrings #-}

module SPL.Main where

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.Process (proc, CreateProcess(..), waitForProcess, withCreateProcess)
import Text.Megaparsec (errorBundlePretty)
import qualified Data.Text.IO as TextIO

import SPL.Parser (file, parse)
import SPL.Printer (render)

cmd :: FilePath -> [String] -> IO ExitCode
cmd bin args = do
  let process = (proc bin args) { delegate_ctlc = True }
  withCreateProcess process (\_ _ _ handle -> waitForProcess handle)

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  program <- TextIO.readFile path

  ast <- case parse file path program of
    Left errorBundle -> do
      putStr $ errorBundlePretty errorBundle
      exitWith $ ExitFailure 1
    Right ast -> pure ast

  TextIO.putStrLn $ render ast
