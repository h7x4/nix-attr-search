module NAS.Cli.Conversion where

import NAS.Conversion.Docbook2txt
-- import NAS.Conversion.Json2nix

import System.Environment (getArgs)

docbook2txtCli :: IO ()
docbook2txtCli = do
  stdin <- getContents
  args <- getArgs
  let colorizedMode = "-C" `elem` args
  putStrLn $ processDocs colorizedMode stdin
  

json2txtCli :: IO ()
json2txtCli = undefined