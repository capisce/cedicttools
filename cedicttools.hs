import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Text hiding (head, map, unfoldr)
import           Data.Text.Normalize
import qualified Data.Text as Text

ex = pack "shao4 nu:3"

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
        maybe syllable id (msum $ map adjustPhthong precedence)

stripTone :: Text -> (Text, Int)
stripTone syllable = (dropEnd 1 syllable, digitToInt $ Text.head $ takeEnd 1 syllable)

fixVowels :: Text -> Text
fixVowels syllable = replace (pack "u:") (pack "ü") syllable

convert :: Text -> [Text]
convert text = let
        next text = do
            let segment = Text.break isSpace $ stripStart text
            guard (not $ Text.null $ fst $ segment)
            return segment

        segments = unfoldr next text
    in map (uncurry applyTone . stripTone . fixVowels) segments

main = putStrLn $ unpack $ Text.intercalate (pack " ") $ convert ex

--main = readFile "cedict_1_0_ts_utf-8_mdbg.txt"
