import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Text hiding (head, map, unfoldr)
import           Data.Text.Normalize
import qualified Data.Text as Text
import qualified Data.Map as M
import           System.IO
import           Text.ParserCombinators.Parsec hiding (spaces)

ex = pack "主場"

data CedictEntry = CedictEntry {
    simplified :: Text,
    traditional :: Text,
    pinyin :: [(Text, Int)],
    translations :: [Text]
} deriving (Show)

data CedictDatabase = CedictDatabase {
    entries :: M.Map Text [CedictEntry]
}

findEntry :: Text -> CedictDatabase -> [CedictEntry]
findEntry key = M.findWithDefault [] key . entries

parser :: Parser CedictEntry
parser =
    let
        spaces = skipMany space
    in do
        simplified <- many (noneOf " ")
        spaces
        traditional <- many (noneOf " ")
        spaces
        char '['
        pinyin <- many (noneOf "]")
        char ']'
        spaces
        char '/'
        translations <- endBy (many (noneOf "/")) (char '/')
        return $ CedictEntry (pack simplified) (pack traditional) (convert $ pack pinyin) (map pack translations)

toneToDiacriticChar :: Int -> Char
toneToDiacriticChar tone =
    let diacritics = map toEnum [0x0304, 0x0301, 0x030C, 0x0300]
    in diacritics !! (tone-1)

applyTone :: Text -> Int -> Text
applyTone syllable tone =
    let precedence = map pack ["a", "o", "e", "iu", "i", "u", "ü"]
        adjustPhthong :: Text -> Maybe Text
        adjustPhthong phthong =
            let splits = splitOn phthong syllable
                diacritic = toneToDiacriticChar tone
            in case splits of
                x:y:tail -> Just $ normalize NFC $ Text.intercalate phthong (x:(cons diacritic y):tail)
                _ -> Nothing
    in
        if (tone < 5) then
            maybe syllable id (msum $ map adjustPhthong precedence)
        else
            syllable

stripTone :: Text -> (Text, Int)
stripTone syllable =
    let
        tone = Text.head $ takeEnd 1 syllable
    in
        if (isDigit tone) then
            (dropEnd 1 syllable, digitToInt tone)
        else
            (syllable, 5)

fixVowels :: Text -> Text
fixVowels syllable = replace (pack "u:") (pack "ü") syllable

convert :: Text -> [(Text, Int)]
convert text = let
        next text = do
            let segment = Text.break isSpace $ stripStart text
            guard (not $ Text.null $ fst $ segment)
            return segment

        segments = unfoldr next text

        conv (text, tone) = (applyTone text tone, tone)
    in map (conv . stripTone . fixVowels) segments

readEntry :: Text -> Either ParseError CedictEntry
readEntry text = parse parser "cedict" (unpack text)

readDatabase :: IO CedictDatabase
readDatabase =
    let
        loop database file = do
            eof <- hIsEOF file
            if eof then
                return database
            else do
                line <- fmap (strip . pack) $ hGetLine file
                if (Text.head line == '#') then
                    loop database file
                else do
                    d' <- either
                        (\e -> putStrLn ("Parse error: " ++ (show line) ++ (show e)) >> return database)
                        (\e -> return (M.insertWith (++) (simplified e) [e] database))
                        (readEntry line)
                    loop d' file
    in
        fmap CedictDatabase $ withFile "cedict_1_0_ts_utf-8_mdbg.txt" ReadMode (loop M.empty)

main = do
    database <- readDatabase
    putStrLn $ show $ findEntry ex database
