module NAS.Utils where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Process (readProcess)

wrapProgram :: String -> [String] -> Maybe String -> IO String
wrapProgram programName args maybeInput = do
  case maybeInput of 
    Just input -> readProcess programName args input
    Nothing -> readProcess programName args ""

exitWithError :: String -> IO a
exitWithError errorMessage = hPutStrLn stderr errorMessage >> exitFailure