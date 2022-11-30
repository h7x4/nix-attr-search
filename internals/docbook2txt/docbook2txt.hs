--   This is a program that converts docbook xml to optionally ANSI colored
--   raw text.
--
--   See https://tdg.docbook.org/ for more information about the docbook format.
--
--   This conversion could also be achieved by using pandoc.
--   However because of the custom color formatting, we would probably
--   end up having to write custom conversion logic for every tag to be
--   consumed by pandoc anyway. So instead, I am just planning on keeping
--   my own module parsing raw xml tags (for now).

{-# LANGUAGE PatternSynonyms #-}

import Data.List (find, intersperse)
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

-- Remove `</para><para>` tags.
-- If there are more in one doc comment, the middle ones
-- will be parsed as a `TagBranch`, which means this
-- at most has to remove one closing and one opening tag.
removeParagraphTags :: [TS.TagTree String] -> [TS.TagTree String]
removeParagraphTags (TS.TagLeaf (TS.TagClose "para") : TS.TagLeaf (TS.TagOpen "para" []) : rest) =
  TS.TagLeaf (TS.TagText "\n") : removeParagraphTags rest
-- In this case, it will be directly followed by a <para> branch
removeParagraphTags (TS.TagLeaf (TS.TagClose "para") : rest) = removeParagraphTags rest
-- In this case, it is directly behind by a <para> branch
removeParagraphTags (TS.TagLeaf (TS.TagOpen "para" _) : rest) = removeParagraphTags rest
removeParagraphTags (x : y : rest) = x : removeParagraphTags (y : rest)
removeParagraphTags x = x

-- Print a list of PCSs.
-- Depending on the first argument, the color can be optionally
-- colored.
printTags :: Bool -> [PotentiallyColorizedString] -> IO ()
printTags False = putStrLn . unwords . map nonColorized
printTags True = mapM_ colorized

-- ANSI helpers

wrapSGR :: AN.SGR -> String -> IO ()
wrapSGR sgr str = do
  AN.setSGR [sgr]
  putStr str
  AN.setSGR [AN.Reset]

wrapColor :: AN.Color -> String -> IO ()
wrapColor c = wrapSGR (AN.SetColor AN.Foreground AN.Vivid c)

wrapTxt :: String -> String -> String
wrapTxt delimiter string = concat [delimiter, string, delimiter]

bold :: AN.SGR
bold = AN.SetConsoleIntensity AN.BoldIntensity

pattern TextLeaf a = TS.TagLeaf (TS.TagText a)

-- Replace tags with their PCS string equivalent.
replaceTagColor :: TS.TagTree String -> PotentiallyColorizedString
replaceTagColor tagtree = case tagtree of
  TS.TagLeaf (TS.TagText s) ->
    PCS
      { colorized = putStr s,
        nonColorized = s
      }
  TS.TagBranch "para" _ inner ->
    PCS
      { colorized = mapM_ (colorized . replaceTagColor) inner,
        nonColorized = concatMap (nonColorized . replaceTagColor) inner
      }
  TS.TagBranch "code" _ [TextLeaf content] ->
    PCS
      { colorized = wrapSGR bold $ wrapTxt "`" content,
        nonColorized = wrapTxt "`" content
      }
  TS.TagBranch "command" _ [TextLeaf content] ->
    PCS
      { colorized = wrapSGR bold $ wrapTxt "`" content,
        nonColorized = wrapTxt "`" content
      }
  TS.TagBranch "filename" _ [TextLeaf content] ->
    PCS
      { colorized = wrapColor AN.Yellow content,
        nonColorized = wrapTxt "`" content
      }
  TS.TagBranch "emphasis" _ [TextLeaf content] ->
    PCS
      { colorized = wrapSGR bold content,
        nonColorized = wrapTxt "`" content
      }
  TS.TagBranch "literal" _ [TextLeaf content] ->
    PCS
      { colorized = wrapColor AN.Red content,
        nonColorized = wrapTxt "`" content
      }
  TS.TagBranch "varname" _ [TextLeaf content] ->
    PCS
      { colorized = wrapColor AN.Red content,
        nonColorized = wrapTxt "`" content
      }
  TS.TagBranch "link" [("xlink:href", link)] [] ->
    PCS
      { colorized = wrapColor AN.Blue link,
        nonColorized = concat ["`", link, "`"]
      }
  TS.TagBranch "link" [("xlink:href", link)] [TextLeaf content] ->
    PCS
      { colorized = wrapColor AN.Blue $ concat [content, " (", link, ")"],
        nonColorized = concat ["`", content, " (", link, ")`"]
      }
  TS.TagBranch "option" _ [TextLeaf content] ->
    PCS
      { colorized = wrapSGR bold $ wrapTxt "`" content,
        nonColorized = wrapTxt "`" content
      }
  TS.TagBranch "envar" _ [TextLeaf content] ->
    PCS
      { colorized = wrapSGR bold $ concat ["`$", content, "`"],
        nonColorized = concat ["`$", content, "`"]
      }
  TS.TagBranch "quote" _ inner ->
    PCS
      { colorized = sequence_ [putStr "\"", mapM_ (colorized . replaceTagColor) inner, putStr "\""],
        nonColorized = wrapTxt "\"" $ concatMap (nonColorized . replaceTagColor) inner
      }
  TS.TagBranch "warning" _ inner ->
    PCS
      { colorized = sequence_ [wrapColor AN.Red "WARNING: ", mapM_ (colorized . replaceTagColor) inner],
        nonColorized = "WARNING: " ++ concatMap (nonColorized . replaceTagColor) inner
      }
  TS.TagBranch "xref" [("linkend", link)] [] ->
    let removeOptPrefix :: String -> String
        removeOptPrefix ('o' : 'p' : 't' : '-' : rest) = rest
        removeOptPrefix x = x

        replaceName :: String -> IO ()
        replaceName x = if x == "_name_" then wrapColor AN.Red "<name>" else wrapSGR bold x

        formattedLink :: [IO ()]
        formattedLink = intersperse (wrapSGR bold ".") $ map replaceName $ splitOn "." $ removeOptPrefix link
     in PCS
          { colorized = sequence_ $ [putStr "`"] ++ formattedLink ++ [putStr "`"],
            nonColorized = wrapTxt "`" link
          }
  TS.TagBranch "citerefentry" _ content ->
    let tagBranchTagMatches :: String -> TS.TagTree String -> Bool
        tagBranchTagMatches x (TS.TagBranch tag _ _) = tag == x
        tagBranchTagMatches _ _ = False

        title :: Maybe String
        title = case find (tagBranchTagMatches "refentrytitle") content of
          Just (TS.TagBranch _ _ [TextLeaf str]) -> Just str
          _ -> Nothing

        volumNum :: Maybe String
        volumNum = case find (tagBranchTagMatches "manvolnum") content of
          Just (TS.TagBranch _ _ [TextLeaf str]) -> Just str
          _ -> Nothing

        combinedLink :: String
        combinedLink = case (title, volumNum) of
          (Just t, Just vn) -> concat [t, "(", vn, ")"]
          (Just t, Nothing) -> t
          _ -> "???"
     in PCS
          { colorized = wrapColor AN.Blue combinedLink,
            nonColorized = combinedLink
          }
  unknown ->
    PCS
      { colorized = wrapColor AN.Red $ TS.renderTree [unknown],
        nonColorized = TS.renderTree [unknown]
      }
