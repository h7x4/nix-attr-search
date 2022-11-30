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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

import Data.Char (isSpace)
import Data.List (find, intersperse)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import qualified System.Console.ANSI.Codes as AN
import qualified System.Console.ANSI.Types as AN
import System.Environment (getArgs)
import qualified Text.HTML.TagSoup as TS
import qualified Text.HTML.TagSoup.Tree as TS
-- import qualified Text.Layout.Table as T
-- import qualified Text.Layout.Table.Cell as T
-- import qualified Text.Layout.Table.Cell.Formatted as T

--
-- Datatypes with relevant methods
--

-- TODO: Mark reflowable text, and do a reflow fold before print.

-- data ShouldReflow a = SRReflow a
--                     | SRConstant a
--   deriving (Show, Eq)

data Formatted
  = FSeveral [Formatted]
  | FWrapped String Formatted String
  | FPlain String
  deriving (Eq)

instance IsString Formatted where
  fromString = FPlain

instance Show Formatted where
  show (FSeveral fs) = concatMap show fs
  show (FWrapped w1 s w2) = concat [w1, show s, w2]
  show (FPlain s) = s

instance Semigroup Formatted where
  FSeveral x <> FSeveral y = FSeveral $ x ++ y
  f1 <> FSeveral x = FSeveral $ f1 : x
  FSeveral x <> f1 = FSeveral $ x ++ [f1]
  f1 <> f2 = FSeveral [f1, f2]

instance Monoid Formatted where
  mempty = FPlain mempty

realString :: Formatted -> String
realString (FSeveral fs) = concatMap realString fs
realString (FWrapped w1 s w2) = realString s
realString (FPlain s) = s

realLength :: Formatted -> Int
realLength = length . realString

-- TODO: Revisit when table-layout gets a new release
-- https://github.com/muesli4/table-layout/issues/43

-- toTableFormattedType :: Formatted -> T.Formatted String
-- toTableFormattedType f = case f of
--   FSeveral fs -> mconcatMap toTableFormattedType fs
--   FWrapped w1 (FPlain s) w2 -> T.formatted w1 s w2
--   FWrapped _ f1 _ -> toTableFormattedType f1
--   FPlain s -> T.plain s

data PotentiallyColorizedString = PCS
  { colorized :: Formatted,
    nonColorized :: String
  }
  deriving (Show, Eq)

instance Semigroup PotentiallyColorizedString where
  pcs1 <> pcs2 =
    PCS
      { colorized = colorized pcs1 <> colorized pcs2,
        nonColorized = nonColorized pcs1 <> nonColorized pcs2
      }

instance Monoid PotentiallyColorizedString where
  mempty =
    PCS
      { colorized = mempty,
        nonColorized = mempty
      }

main :: IO ()
main = do
  stdin <- getContents
  args <- getArgs
  let colorizedMode = "-C" `elem` args
  printTags colorizedMode $ map replaceTagColor $ removeParagraphTags $ TS.parseTree stdin

-- Print a list of PCSs.
-- Depending on the first argument, the color can be optionally
-- colored.
printTags :: Bool -> [PotentiallyColorizedString] -> IO ()
printTags False = putStrLn . unwords . map nonColorized
printTags True = putStrLn . mconcatMap (show . colorized)

-- ANSI helpers

wrapSGR :: AN.SGR -> String -> Formatted
wrapSGR sgr str = FWrapped (AN.setSGRCode [sgr]) (fromString str) (AN.setSGRCode [AN.Reset])

wrapColor :: AN.Color -> String -> Formatted
wrapColor = wrapSGR . AN.SetColor AN.Foreground AN.Vivid

wrapTxt :: String -> String -> String
wrapTxt delimiter string = concat [delimiter, string, delimiter]

wrapTxt' :: String -> Formatted -> Formatted
wrapTxt' delimiter string = FSeveral [fromString delimiter, string, fromString delimiter]

bold :: AN.SGR
bold = AN.SetConsoleIntensity AN.BoldIntensity

mconcatMap :: Monoid b => (a -> b) -> [a] -> b
mconcatMap f = mconcat . map f

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

pattern TextLeaf a = TS.TagLeaf (TS.TagText a)

-- Replace tags with their PCS string equivalent.
replaceTagColor :: TS.TagTree String -> PotentiallyColorizedString
replaceTagColor tagtree = case tagtree of
  TS.TagLeaf (TS.TagText s) ->
    PCS
      { colorized = fromString s,
        nonColorized = s
      }
  TS.TagBranch "para" _ inner ->
    PCS
      { colorized = mconcatMap (colorized . replaceTagColor) inner,
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
      { colorized = wrapTxt' "\"" $ mconcatMap (colorized . replaceTagColor) inner,
        nonColorized = wrapTxt "\"" $ concatMap (nonColorized . replaceTagColor) inner
      }
  TS.TagBranch "warning" _ inner ->
    PCS
      { colorized = mconcat [wrapColor AN.Red "WARNING: ", mconcatMap (colorized . replaceTagColor) inner],
        nonColorized = "WARNING: " ++ concatMap (nonColorized . replaceTagColor) inner
      }
  TS.TagBranch "xref" [("linkend", link)] [] ->
    let removeOptPrefix :: String -> String
        removeOptPrefix ('o' : 'p' : 't' : '-' : rest) = rest
        removeOptPrefix x = x

        replaceName :: String -> Formatted
        replaceName "_name_" = wrapColor AN.Red "<name>"
        replaceName x = wrapSGR bold x

        formattedLink :: Formatted
        formattedLink = mconcat $ intersperse (wrapSGR bold ".") $ map replaceName $ splitOn "." $ removeOptPrefix link
     in PCS
          { colorized = wrapTxt' "`" formattedLink,
            nonColorized = wrapTxt "`" link
          }
  -- TS.TagBranch "informaltable" _ [inner] ->
  --   let
  --     extractRows :: TS.TagTree String -> Maybe [TS.TagTree String]
  --     extractRows (TS.TagBranch "tgroup" _ [TS.TagBranch "tbody" _ rows]) = Just rows
  --     extractRows _ = Nothing

  --     -- TODO: This filters too much
  --     isWhiteSpace :: TS.TagTree String -> Bool
  --     isWhiteSpace (TextLeaf s) = False
  --     isWhiteSpace _ = True

  --     parseRow :: TS.TagTree String -> Maybe [PotentiallyColorizedString]
  --     parseRow (TS.TagBranch "row" _ entries) = sequence $ map parseEntry $ filter isWhiteSpace entries
  --     parseRow _ = Nothing

  --     parseEntry :: TS.TagTree String -> Maybe PotentiallyColorizedString
  --     parseEntry (TS.TagBranch "entry" _ content) = Just $ mconcatMap replaceTagColor content
  --     parseEntry _ = Nothing

  --     rawRows :: Maybe [[PotentiallyColorizedString]]
  --     rawRows = do
  --       rows <- extractRows inner
  --       sequence $ map parseRow $ filter isWhiteSpace rows

  --     generateColSpec :: [[a]] -> [T.ColSpec]
  --     generateColSpec rs = flip replicate T.def $ length $ rs !! 0

  --     generateTableConfig :: T.Cell a => [[b]] -> [T.RowGroup a] -> String
  --     generateTableConfig rs = (++) "\n" . T.tableString (generateColSpec rs) T.unicodeRoundS T.def

  --     table :: T.Cell a => (PotentiallyColorizedString -> a) -> Maybe String
  --     table f = case rawRows of
  --       Nothing -> Nothing
  --       Just rrs -> Just $ generateTableConfig rrs $ map (T.rowG . map f) $ rrs

  --     errorMessage :: String
  --     errorMessage = "ERROR: Could not parse <informaltable>";
  --   in PCS {
  --     colorized = fromMaybe (wrapColor AN.Red errorMessage) (FPlain <$> table (toTableFormattedType . colorized)),
  --     nonColorized = fromMaybe errorMessage $ table nonColorized
  --   }
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
