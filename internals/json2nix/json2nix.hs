{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value(..), decode)
import Data.Aeson.Key (Key, toString)
import Data.Aeson.KeyMap (foldMapWithKey)
import Data.Aeson.Parser (json')
import Data.Aeson.Types (parse)
import Data.String (fromString)
import Data.Text (Text(..), unpack, replace, isInfixOf)
import Data.Vector (Vector)
import Data.Maybe (fromMaybe)

main :: IO ()
main = interact f
  where
    f input = case decode $ fromString input of
      Nothing -> "json2nix error - could not parse input\n" ++ show input
      Just jsonValue -> json2Nix jsonValue

keyValToString :: Key -> Value -> String
keyValToString key value = toString key ++ " = " ++ json2Nix value ++ ";"

-- escapeDollar :: Text -> Text
-- escapeDollar = replace "''${" "\\''${"

json2Nix :: Value -> String
json2Nix (Object object) =  "{" ++ foldMapWithKey keyValToString object ++ "}"
json2Nix (Array array) = "[" ++  foldr (\x y -> x ++ " " ++ y) "" (fmap json2Nix array) ++ "]"
json2Nix (Number n) = show n
json2Nix (Bool False) = "false"
json2Nix (Bool True) = "true"
json2Nix Null = "null"
json2Nix (String text) = sep ++ unpack text  ++ sep
  where
    sep = if "\"" `isInfixOf` text then "\'\'" else "\""
