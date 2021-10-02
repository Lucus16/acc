{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import qualified Data.Text.IO as TextIO
import System.Environment (getArgs, getEnv)
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

exitOnFailure :: IO ExitCode -> IO ()
exitOnFailure a = a >>= \n -> unless (n == ExitSuccess) (exitWith n)

main :: IO ()
main = do
  args <- getArgs
  libc <- getEnv "LIBC"
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
      objPath = replaceExtension path "o"
      binPath = dropExtension path
  TextIO.writeFile asmPath asm
  exitOnFailure $ cmd "nasm" ["-f", "elf64", "-gdwarf", asmPath, "-o", objPath]
  exitOnFailure $ cmd "ld"
    [ "-o", binPath
    , "-dynamic-linker", libc <> "/lib/ld-linux-x86-64.so.2"
    , libc <> "/lib/crt1.o"
    , libc <> "/lib/crti.o"
    , "-lc"
    , objPath
    , libc <> "/lib/crtn.o"
    ]
