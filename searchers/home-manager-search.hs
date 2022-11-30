{-# LANGUAGE DeriveDataTypeable, RecordWildCards, NamedFieldPuns, DeriveGeneric, CPP, TemplateHaskell, LambdaCase #-}

import System.Console.CmdArgs
import System.IO
import System.Process
import System.Exit
import Data.Map
import Data.Maybe (isNothing)
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data Args = MainArgs
  { json :: Bool
  , no_color :: Bool
  , no_preview :: Bool
  , flake :: Maybe String
  , ref :: Maybe String
  }
  |  PreviewArgs
  { json :: Bool
  , no_color :: Bool
  , no_preview :: Bool
  , datasource :: Maybe String
  }
  deriving (Data,Typeable,Show,Eq)

mainArgTemplate :: Args
mainArgTemplate = MainArgs
  { json = False &= help "Show JSON data in preview pane"
  , no_color = False &= help "Don't display ANSI colors in preview pane"
  , no_preview = False &= help "Don't display preview pane"
  , flake = Nothing &= typ "FLAKE_URI" &= groupname "source" &= help "Specify home-manager flake to show manual from"
  , ref = Nothing &= typ "REF" &= groupname "source" &= help "Specify git reference for the flake path to show manual from (if applicable)"
  }
  &= auto

defaultHMSource :: String
defaultHMSource = HOME_MANAGER_DEFAULT_PATH

wrapProgram :: String -> [String] -> Maybe String -> IO String
wrapProgram programName args maybeInput = do
  case maybeInput of 
    Just input -> readProcess programName args input
    Nothing -> readProcess programName args ""
  -- (mStdin, mStdout, _, _) <- createProcess $ proc programName args
  -- case (mStdin, maybeInput) of
  --   (Just stdin, Just input) -> hPutStr stdin input
  --   _ -> return ()
  -- sequence $ hGetContents <$> mStdout

exitWithError :: String -> IO a
exitWithError errorMessage = hPutStrLn stderr errorMessage >> exitFailure

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
  return $ unlines . keys <$> (A.eitherDecode json :: Either String (Map String A.Object))

search :: Args -> IO ()
search args = do
  maybeJsonOptionsPath <- case flake args of
    Nothing -> return $ Right defaultHMSource
    Just flk -> flip fetchHMSource (ref args) flk

  jsonOptionsPath <- case maybeJsonOptionsPath of
    Left err -> exitWithError $ "Could not fetch json source\n" ++ err
    Right src -> return src

  optionList <- parseOptionList jsonOptionsPath >>= \case
    Left err -> exitWithError $ "Could not parse options:\n" ++ err
    Right opts -> return opts
  
  chosenValue <- wrapProgram "fzf" ["--preview", "home-manager-search preview"] $ Just optionList

  case chosenValue of 
    "" -> return ()
    v -> putStrLn v

-- TODO: Make this invisible

previewTemplate :: Args
previewTemplate = PreviewArgs
  { json = False
  , no_color = False
  , no_preview = False
  , datasource = Nothing &= typFile
  }
  &= help "This is a mode that should only be used internally. Please see the main usage"
  -- &= helpArg [explicit, name "help", name "h"]

preview :: Args -> IO ()
preview args = do
  putStrLn "Hello!"

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
    MainArgs {..} -> search args 
    PreviewArgs {..} -> preview args
-- preview :: IO ()




  -- json <- wrapProgram "jq" [".", defaultHMSource] Nothing
  -- case json of
  --   Just j -> putStrLn j
  --   Nothing -> return ()
  -- (_, sout, _, _) <- createProcess $ shell $ "cat " ++ defaultHMSource ++  " | jq -r '.description'"

  -- putStrLn defaultHMSource



      -- JSON_DATA=$(${jq} ".\"$OPTION_KEY\"" $JSON_MANUAL_PATH)
      -- export DESCRIPTION=$(echo $JSON_DATA | ${jq} -r ".description" | ${docbook2txt}/bin/docbook2txt ${docbook2txtColorArg})

      -- EXAMPLE_DATA=$(echo $JSON_DATA | ${jq} -r ".example.text" 2>/dev/null | ${nixfmt} 2>/dev/null)
      -- if [ $? != 0 ]; then
      --   EXAMPLE_DATA=$(echo $JSON_DATA | ${jq} ".example" | ${json2nix}/bin/json2nix)
      -- fi
      -- export EXAMPLE=$(echo $EXAMPLE_DATA | ${bat} ${batColorArg}--style=numbers -lnix)

      -- export DEFAULT=$(echo $JSON_DATA | ${jq} ".default" | ${json2nix}/bin/json2nix | ${bat} ${batColorArg}--style=numbers -lnix)
      -- echo $JSON_DATA | ${gomplate} --datasource opt=stdin:?type=application/json --file ${template}