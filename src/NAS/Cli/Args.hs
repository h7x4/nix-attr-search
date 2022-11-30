{-# LANGUAGE DeriveDataTypeable #-}

module NAS.Cli.Args (Args(..)) where

import Data.Data
 
data Args = MainArgs
  { json :: Bool
  , no_color :: Bool
  , no_preview :: Bool
  , flake :: Maybe String
  , ref :: Maybe String
  }
  | PreviewArgs
  { optionKey :: String
  , base64EncodedOptions :: String
  }
  -- , no_color :: Bool
  -- , no_preview :: Bool
  -- , datasource :: Maybe String
  -- }
  deriving (Data,Typeable,Show,Eq)