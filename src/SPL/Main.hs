{-# LANGUAGE OverloadedStrings #-}

module SPL.Main where

import Data.Foldable (for_)
import Data.Traversable (for)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.Process (proc, CreateProcess(..), waitForProcess, withCreateProcess)
import Text.Megaparsec (errorBundlePretty)
import qualified Data.Text.IO as Text

import SPL.Parser (file, parse)
import Pretty (render)
import SPL.Resolver (resolve)

cmd :: FilePath -> [String] -> IO ExitCode
cmd bin args = do
  let process = (proc bin args) { delegate_ctlc = True }
  withCreateProcess process (\_ _ _ handle -> waitForProcess handle)

main :: IO ()
main = do
  args <- getArgs
  ast <- fmap concat $ for args $ \path -> do
    program <- Text.readFile path

    case parse file path program of
      Left errorBundle -> do
        putStr $ errorBundlePretty errorBundle
        exitWith $ ExitFailure 1
      Right ast -> pure ast

  case resolve ast of
    Left errs -> for_ errs $ Text.putStrLn . render
    Right (resolved, _) -> Text.putStrLn $ render resolved
