{-# LANGUAGE OverloadedStrings #-}
module CedictTools
    ( CedictEntry
    , CedictTools
    , initCedictTools
    , match
    , pinyin
    , showEntry
    , simplified
    , traditional
    , translations
    ) where

import           Control.Arrow
import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.Char
import           Data.List
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Text (Text)
import           Data.Trie (Trie)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Normalize as Text
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Trie as Trie
import           System.IO
import           Text.ParserCombinators.Parsec hiding (spaces)

ex = "主場"

data CedictEntry = CedictEntry {
    simplified :: Text,
    traditional :: Text,
    pinyin :: [(Text, Int)],
    translations :: [Text]
} deriving (Show)

data CedictTools = CedictTools {
    trie :: Trie [CedictEntry]
}

data CedictDatabase = CedictDatabase {
    entries :: Map Text [CedictEntry]
}

makeTrie :: CedictDatabase -> CedictTools
makeTrie database =
    CedictTools $ Trie.fromList $ Map.toList (Map.mapKeys (Text.encodeUtf8) (entries database))

match :: CedictTools -> Text -> Maybe ((Text, [CedictEntry]), Text)
match tools str =
    fmap (\(prefix, entry, remaining) -> ((Text.decodeUtf8 prefix, entry), Text.decodeUtf8 remaining)) $
        Trie.match (trie tools) (Text.encodeUtf8 str)

consume :: CedictTools -> Text -> [(Text, [CedictEntry])]
consume tools str =
    unfoldr (match tools) str

findEntries :: CedictDatabase -> Text -> [CedictEntry]
findEntries database key =
    Map.findWithDefault [] key (entries database)

parser :: Parser CedictEntry
parser =
    let
        spaces = skipMany space
    in do
        traditional <- many (noneOf " ")
        spaces
        simplified <- many (noneOf " ")
        spaces
        char '['
        pinyin <- many (noneOf "]")
        char ']'
        spaces
        char '/'
        translations <- endBy (many (noneOf "/")) (char '/')
        return $
            CedictEntry {
                simplified = Text.pack simplified,
                traditional = Text.pack traditional,
                pinyin = convert $ Text.pack pinyin,
                translations = map Text.pack translations
            }

toneToDiacriticChar :: Int -> Char
toneToDiacriticChar tone =
    let diacritics = map toEnum [0x0304, 0x0301, 0x030C, 0x0300]
    in diacritics !! (tone-1)

applyTone :: Text -> Int -> Text
applyTone syllable tone =
    let precedence = ["a", "o", "e", "iu", "i", "u", "ü"]
        adjustPhthong :: Text -> Maybe Text
        adjustPhthong phthong =
            let splits = Text.splitOn phthong syllable
                diacritic = toneToDiacriticChar tone
            in case splits of
                x:y:tail -> Just $ Text.normalize Text.NFC
                                 $ Text.intercalate phthong
                                    (x:(Text.cons diacritic y):tail)
                _ -> Nothing
    in
        if (tone < 5) then
            maybe syllable id (msum $ map adjustPhthong precedence)
        else
            syllable

stripTone :: Text -> (Text, Int)
stripTone syllable =
    let
        tone = Text.head $ Text.takeEnd 1 syllable
    in
        if (isDigit tone) then
            (Text.dropEnd 1 syllable, digitToInt tone)
        else
            (syllable, 5)

fixVowels :: Text -> Text
fixVowels syllable =
    Text.replace "u:" "ü" syllable

convert :: Text -> [(Text, Int)]
convert text =
    let
        next text = do
            let segment = Text.break isSpace $ Text.stripStart text
            guard (not $ Text.null $ fst $ segment)
            return segment

        segments = unfoldr next text

        conv (text, tone) = (applyTone text tone, tone)
    in map (conv . stripTone . fixVowels) segments

readEntry :: Text -> Either ParseError CedictEntry
readEntry text =
    parse parser "cedict" (Text.unpack text)

parseError :: Text -> ParseError -> IO ()
parseError line error =
    putStrLn $ "Parse error: " ++ show line ++ show error

initCedictTools :: String -> IO CedictTools
initCedictTools cedictFile =
    do
        database <- readDatabase cedictFile
        return $ makeTrie database

readDatabase :: String -> IO CedictDatabase
readDatabase cedictFile =
    do
        database <- withFile cedictFile ReadMode (loop Map.empty)
        return $ CedictDatabase database
    where
        insert database entry =
            Set.foldr
                (\key -> Map.insertWith (++) key [entry])
                database
                (Set.fromList [simplified entry, traditional entry])
        loop database file = do
            eof <- hIsEOF file
            if eof then
                return database
            else do
                line <- fmap (Text.strip . Text.pack) $ hGetLine file
                if (Text.head line == '#') then
                    loop database file
                else do
                    d' <- either
                        (\e -> parseError line e >> return database)
                        (\e -> return (insert database e))
                        (readEntry line)
                    loop d' file

showEntry :: CedictEntry -> String
showEntry entry =
    (Text.unpack $ simplified entry) ++ "/"
    ++ (Text.unpack $ traditional entry) ++ "/"
    ++ (concat $ intersperse " " $ map (Text.unpack . fst) $ pinyin entry) ++ "/"
    ++ (concat $ intersperse ", " $ map Text.unpack $ translations entry)

process :: CedictTools -> Text -> IO ()
process tools str =
    forM_ (consume tools str) $
        \(text, entries) -> do
        putStrLn $ Text.unpack text
        mapM_ (putStrLn . showEntry) entries

cedictFile = "cedict_1_0_ts_utf-8_mdbg.txt"

main =
    do
        tools <- initCedictTools cedictFile
        input <- getContents
        mapM_ (process tools . Text.pack) (lines input)
