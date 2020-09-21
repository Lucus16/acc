{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath.Posix (dropExtension, replaceExtension)
import System.Process (proc, CreateProcess(..), waitForProcess, withCreateProcess)
import Text.Megaparsec (errorBundlePretty, parse)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

import Assembler (compile)
import C (fdef)

callProcess :: FilePath -> [String] -> IO ExitCode
callProcess cmd args = do
  let process = (proc cmd args) { delegate_ctlc = True }
  withCreateProcess process (\_ _ _ handle -> waitForProcess handle)

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  program <- TextIO.readFile path
  case parse fdef path program of
    Left errorBundle -> do
      putStr $ errorBundlePretty errorBundle
      exitWith $ ExitFailure 1
    Right ast -> do
      let asmPath = replaceExtension path "s"
      let binPath = dropExtension path
      TextIO.writeFile asmPath $ compile ast
      exitCode <- callProcess "gcc" [asmPath, "-o", binPath]
      exitWith exitCode
