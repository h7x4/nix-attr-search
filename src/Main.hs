{-# LANGUAGE NamedFieldPuns #-}

module Main where

import NAS.Cli.Args
import NAS.Searchers.HomeManager (search, mainArgTemplate)
import NAS.Preview (preview)

import System.Console.CmdArgs
import Data.Map

previewTemplate :: Args
previewTemplate = PreviewArgs { optionKey = def &= argPos 0
                              , base64EncodedOptions = def &= argPos 1
                              }
                              &= help "This is a mode that should only be used internally. Please see the main usage"

argModes :: Mode (CmdArgs Args)
argModes = cmdArgsMode $ modes [mainArgTemplate, previewTemplate]
  &= help ""
  &= verbosity
  &= helpArg [explicit, name "help", name "h"]
  &= summary "nix-attr-search v1.0.0"

main :: IO ()
main = do
  args <- cmdArgsRun argModes
  case args of
    MainArgs {} -> search args 
    PreviewArgs {} -> preview (optionKey args) (base64EncodedOptions args)