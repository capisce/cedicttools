{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text.Normalize as Text
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import           System.IO
import           Text.ParserCombinators.Parsec hiding (spaces)

ex = "主場"

data CedictEntry = CedictEntry {
    simplified :: Text,
    traditional :: Text,
    pinyin :: [(Text, Int)],
    translations :: [Text]
} deriving (Show)

data CedictDatabase = CedictDatabase {
    entries :: Map Text [CedictEntry]
}

findEntries :: CedictDatabase -> Text -> [CedictEntry]
findEntries database key = Map.findWithDefault [] key (entries database)

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
            CedictEntry
                (Text.pack simplified) (Text.pack traditional)
                (convert $ Text.pack pinyin) (map Text.pack translations)

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
                x:y:tail -> Just $ Text.normalize Text.NFC $ Text.intercalate phthong (x:(Text.cons diacritic y):tail)
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
fixVowels syllable = Text.replace "u:" "ü" syllable

convert :: Text -> [(Text, Int)]
convert text = let
        next text = do
            let segment = Text.break isSpace $ Text.stripStart text
            guard (not $ Text.null $ fst $ segment)
            return segment

        segments = unfoldr next text

        conv (text, tone) = (applyTone text tone, tone)
    in map (conv . stripTone . fixVowels) segments

readEntry :: Text -> Either ParseError CedictEntry
readEntry text = parse parser "cedict" (Text.unpack text)

readDatabase :: IO CedictDatabase
readDatabase =
    let
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
                        (\e -> putStrLn ("Parse error: " ++ (show line) ++ (show e)) >> return database)
                        (\e -> return (insert database e))
                        (readEntry line)
                    loop d' file
    in
        fmap CedictDatabase $ withFile "cedict_1_0_ts_utf-8_mdbg.txt" ReadMode (loop Map.empty)

showEntry :: CedictEntry -> String
showEntry entry =
    (Text.unpack $ simplified entry) ++ "/"
    ++ (Text.unpack $ traditional entry) ++ "/"
    ++ (concat $ intersperse " " $ map (Text.unpack . fst) $ pinyin entry) ++ "/"
    ++ (concat $ intersperse ", " $ map Text.unpack $ translations entry)

process :: CedictDatabase -> String -> IO ()
process database str =
    case findEntries database (Text.pack str) of
        [] -> hPutStrLn stderr $ "No match found for " ++ str
        entries -> mapM_ (putStrLn . showEntry) entries

main = do
    database <- readDatabase
    input <- getContents
    mapM_ (process database) (lines input)
