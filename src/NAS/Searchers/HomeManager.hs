{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE DeriveGeneric #-}

module NAS.Searchers.HomeManager where

import qualified NAS.Cli.Args as Arg
import NAS.Utils
import NAS.DefaultPaths (defaultHomeManagerOptionsPath)
import NAS.Preview (PreviewOptions(..))

import System.Console.CmdArgs
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import System.IO
import System.Process
import Data.Map as M
import Data.Text.Encoding.Base64

mainArgTemplate :: Arg.Args
mainArgTemplate = Arg.MainArgs
  { Arg.json = False &= help "Show JSON data in preview pane"
  , Arg.no_color = False &= help "Don't display ANSI colors in preview pane"
  , Arg.no_preview = False &= help "Don't display preview pane"
  , Arg.flake = Nothing &= typ "FLAKE_URI" &= groupname "source" &= help "Specify home-manager flake to show manual from"
  , Arg.ref = Nothing &= typ "REF" &= groupname "source" &= help "Specify git reference for the flake path to show manual from (if applicable)"
  }
  &= auto

fetchHMSource :: String -> Maybe String -> IO (Either String String)
fetchHMSource flake ref = undefined

data Option = Option {
    description :: Either String A.Object
  , defaultValue :: Either String A.Object
  , example :: Either String A.Object
  , loc :: [String]
  }
  deriving (Show)

$(A.deriveJSON A.defaultOptions {A.fieldLabelModifier = \x -> if x == "default" then "defaultValue" else x} ''Option)

parseOptionList :: String -> IO (Either String String)
parseOptionList path = do
  json <- BSL.readFile path
  return $ unlines . keys <$> (A.eitherDecode json :: Either String (M.Map String A.Object))

search :: Arg.Args -> IO ()
search args = do
  maybeJsonOptionsPath <- case Arg.flake args of
    Nothing -> return $ Right defaultHomeManagerOptionsPath
    Just flk -> flip fetchHMSource (Arg.ref args) flk

  jsonOptionsPath <- case maybeJsonOptionsPath of
    Left err -> exitWithError $ "Could not fetch json source\n" ++ err
    Right src -> return src

  optionList <- parseOptionList jsonOptionsPath >>= \case
    Left err -> exitWithError $ "Could not parse options:\n" ++ err
    Right opts -> return opts
  
  let base64EncodedOptions = T.unpack $ encodeBase64 $ T.pack $ show $ PreviewOptions {
    json = Arg.json args
    , no_color = Arg.no_color args
    , no_preview = Arg.no_preview args
    , datasource = Just jsonOptionsPath
    }

  chosenValue <- wrapProgram "fzf" ["--preview", "nix-attr-search preview {} " ++ base64EncodedOptions] $ Just optionList

  case chosenValue of 
    "" -> return ()
    v -> putStrLn v