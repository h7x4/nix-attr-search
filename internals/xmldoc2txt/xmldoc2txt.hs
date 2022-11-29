import Data.List (intersperse)
import Data.List.Split (splitOn)
import qualified System.Console.ANSI as AN
import qualified System.Console.ANSI.Types as AN
import System.Environment (getArgs)
import qualified Text.HTML.TagSoup as TS
import qualified Text.HTML.TagSoup.Tree as TS

main :: IO ()
main = do
  stdin <- getContents
  args <- getArgs
  let colorizedMode = "-C" `elem` args
  printTags colorizedMode $ map replaceTagColor $ removeParagraphTags $ TS.parseTree stdin

data PotentiallyColorizedString = PCS
  { colorized :: IO (),
    nonColorized :: String
  }

removeParagraphTags :: [TS.TagTree String] -> [TS.TagTree String]
removeParagraphTags (TS.TagLeaf (TS.TagClose "para") : TS.TagLeaf (TS.TagOpen "para" []) : rest) =
  TS.TagLeaf (TS.TagText "\n") : removeParagraphTags rest
removeParagraphTags (x : y : rest) = x : removeParagraphTags (y : rest)
removeParagraphTags x = x

printTags :: Bool -> [PotentiallyColorizedString] -> IO ()
printTags False = putStrLn . unwords . map nonColorized
printTags True = mapM_ colorized

wrapSGR :: AN.SGR -> String -> IO ()
wrapSGR sgr str = do
  AN.setSGR [sgr]
  putStr str
  AN.setSGR [AN.Reset]

wrapColor :: AN.Color -> String -> IO ()
wrapColor c = wrapSGR (AN.SetColor AN.Foreground AN.Vivid c)

bold :: AN.SGR
bold = AN.SetConsoleIntensity AN.BoldIntensity

replaceTagColor :: TS.TagTree String -> PotentiallyColorizedString
replaceTagColor (TS.TagLeaf (TS.TagText s)) =
  PCS
    { colorized = putStr s,
      nonColorized = s
    }
replaceTagColor (TS.TagBranch "code" _ [TS.TagLeaf (TS.TagText content)]) =
  PCS
    { colorized = wrapSGR bold $ concat ["`", content, "`"],
      nonColorized = concat ["`", content, "`"]
    }
replaceTagColor (TS.TagBranch "command" _ [TS.TagLeaf (TS.TagText content)]) =
  PCS
    { colorized = wrapSGR bold $ concat ["`", content, "`"],
      nonColorized = concat ["`", content, "`"]
    }
replaceTagColor (TS.TagBranch "filename" _ [TS.TagLeaf (TS.TagText content)]) =
  PCS
    { colorized = wrapColor AN.Yellow content,
      nonColorized = concat ["`", content, "`"]
    }
replaceTagColor (TS.TagBranch "emphasis" _ [TS.TagLeaf (TS.TagText content)]) =
  PCS
    { colorized = wrapSGR bold content,
      nonColorized = concat ["`", content, "`"]
    }
replaceTagColor (TS.TagBranch "literal" _ [TS.TagLeaf (TS.TagText content)]) =
  PCS
    { colorized = wrapColor AN.Red content,
      nonColorized = concat ["`", content, "`"]
    }
replaceTagColor (TS.TagBranch "varname" _ [TS.TagLeaf (TS.TagText content)]) =
  PCS
    { colorized = wrapColor AN.Red content,
      nonColorized = concat ["`", content, "`"]
    }
replaceTagColor (TS.TagBranch "link" [("xlink:href", link)] []) =
  PCS
    { colorized = wrapColor AN.Blue link,
      nonColorized = concat ["`", link, "`"]
    }
replaceTagColor (TS.TagBranch "link" [("xlink:href", link)] [TS.TagLeaf (TS.TagText content)]) =
  PCS
    { colorized = wrapColor AN.Blue $ concat [content, " (", link, ")"],
      nonColorized = concat ["`", content, " (", link, ")`"]
    }
replaceTagColor (TS.TagBranch "option" _ [TS.TagLeaf (TS.TagText content)]) =
  PCS
    { colorized = wrapSGR bold $ concat ["`", content, "`"],
      nonColorized = concat ["`", content, "`"]
    }
replaceTagColor (TS.TagBranch "xref" [("linkend", link)] []) =
  PCS
    { colorized = sequence_ $ [putStr "`"] ++ formattedLink ++ [putStr "`"],
      nonColorized = concat ["`", link, "`"]
    }
  where
    removeOptPrefix :: String -> String
    removeOptPrefix ('o' : 'p' : 't' : '-' : rest) = rest
    removeOptPrefix x = x

    replaceName :: String -> IO ()
    replaceName x = if x == "_name_" then wrapColor AN.Red "<name>" else wrapSGR bold x

    formattedLink :: [IO ()]
    formattedLink = intersperse (wrapSGR bold ".") $ map replaceName $ splitOn "." $ removeOptPrefix link
replaceTagColor unknown =
  PCS
    { colorized = wrapColor AN.Red $ TS.renderTree [unknown],
      nonColorized = TS.renderTree [unknown]
    }
