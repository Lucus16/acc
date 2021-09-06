{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TextIO
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath.Posix (dropExtension, replaceExtension)
import System.Process
  (CreateProcess(..), proc, waitForProcess, withCreateProcess)
import Text.Megaparsec (errorBundlePretty, parse)

import Assembler (compile)
import Parser (file)

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

  asm <- case compile ast of
    Left e -> TextIO.putStrLn e >> exitWith (ExitFailure 1)
    Right asm -> pure asm

  let asmPath = replaceExtension path "s"
  let binPath = dropExtension path
  TextIO.writeFile asmPath asm
  exitCode <- cmd "gcc" [asmPath, "-o", binPath]
  exitWith exitCode
