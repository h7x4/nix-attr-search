{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.ByteString as BS
import qualified Data.Either.Extra as E
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Nixfmt (format)

main :: IO ()
main = BS.interact parseNixCode

parseNixCode :: BS.ByteString -> BS.ByteString
parseNixCode input = T.encodeUtf8 $ E.fromEither formattedNixCode
  where
    addErrPrefix :: T.Text -> Either String a -> Either T.Text a
    addErrPrefix prefix = E.mapLeft (T.append prefix . T.pack)

    rawNixCode :: Either T.Text T.Text
    rawNixCode = json2Nix <$> addErrPrefix "json2nix json error -\n" (A.eitherDecode $ BS.fromStrict input)

    formattedNixCode :: Either T.Text T.Text
    formattedNixCode = (addErrPrefix "json2nix nixfmt error -\n" . format 80 "") =<< rawNixCode

keyValToString :: A.Key -> A.Value -> T.Text
keyValToString key value = T.concat [T.pack $ show key, " = ", json2Nix value, ";"]

-- escapeDollar :: T.Text -> T.Text
-- escapeDollar = T.replace "''${" "\\''${"

json2Nix :: A.Value -> T.Text
json2Nix (A.Array array) = T.concat ["[", T.intercalate " " $ V.toList $ fmap json2Nix array, "]"]
json2Nix (A.Object object) = T.concat ["{", A.foldMapWithKey keyValToString object, "}"]
json2Nix (A.Number n) = T.pack $ show n
json2Nix (A.Bool False) = "false"
json2Nix (A.Bool True) = "true"
json2Nix A.Null = "null"
-- Nixfmt will handle which quotation mark we are using anyway, so we
-- might as well just escape all "s and use them as string markers.
json2Nix (A.String text) = T.concat ["\"", T.replace "\"" "\\\"" text, "\""]
