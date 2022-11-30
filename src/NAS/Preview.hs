module NAS.Preview where

import qualified NAS.Cli.Args as A
import NAS.Templates.OptionsTemplate
import NAS.Utils
-- import NAS.Conversion.Json2nix
-- import NAS.Conversion.Docbook2txt

import System.Console.CmdArgs ((&=), help, typFile)
import Data.Text.Encoding.Base64
import Data.Text

-- TODO: Make this invisible

data PreviewOptions =
  PreviewOptions { json :: Bool
                 , no_color :: Bool
                 , no_preview :: Bool
                 , datasource :: Maybe String
                 }
  deriving (Show, Read, Eq)

-- previewTemplate :: Args
-- previewTemplate = PreviewArgs
--   { json = False
--   , no_color = False
--   , no_preview = False
--   , datasource = Nothing &= typFile
--   }
--   

preview :: String -> String -> IO ()
preview searchKey base64EncodedOptions = do
  options <- case decodeBase64 $ pack base64EncodedOptions of
    Right options -> return $ (read :: String -> PreviewOptions) $ unpack options
    Left e -> exitWithError $ "Internal error: could not parse options\n" ++ (unpack e)
  print options
  putStrLn "Hello!"

-- JSON_DATA=$(${jq} ".\"$OPTION_KEY\"" $JSON_MANUAL_PATH)
-- export DESCRIPTION=$(echo $JSON_DATA | ${jq} -r ".description" | ${docbook2txt}/bin/docbook2txt ${docbook2txtColorArg})

-- EXAMPLE_DATA=$(echo $JSON_DATA | ${jq} -r ".example.text" 2>/dev/null | ${nixfmt} 2>/dev/null)
-- if [ $? != 0 ]; then
--   EXAMPLE_DATA=$(echo $JSON_DATA | ${jq} ".example" | ${json2nix}/bin/json2nix)
-- fi
-- export EXAMPLE=$(echo $EXAMPLE_DATA | ${bat} ${batColorArg}--style=numbers -lnix)

-- export DEFAULT=$(echo $JSON_DATA | ${jq} ".default" | ${json2nix}/bin/json2nix | ${bat} ${batColorArg}--style=numbers -lnix)
-- echo $JSON_DATA | ${gomplate} --datasource opt=stdin:?type=application/json --file ${template}