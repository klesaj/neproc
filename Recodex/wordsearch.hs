import Data.List (isPrefixOf, tails, transpose)
import Data.Char (toLower)

search :: [String] -> [String] -> [Int]
search rows words = map countForWord wordsLower
  where
    -- normalize the input: convert to lowercase
    gridLower    = map (map toLower) rows
    wordsLower   = map (map toLower) words

    -- concat all vertical and horizontal lines
    horizontal   = gridLower
    horizontalRev= map reverse gridLower
    vertical     = transpose gridLower
    verticalRev  = map reverse vertical

    allLines     = concat [ horizontal, horizontalRev, vertical, verticalRev]

    -- count occurrences of a word in all lines
    countForWord :: String -> Int
    countForWord w = sum (map (countInLine w) allLines)

    -- count occurrences of a word in a single line
    countInLine :: String -> String -> Int
    countInLine word line = length matches
      where
        suffixes = tails line
        matches  = [ ()| suffix <- suffixes, word `isPrefixOf` suffix ]
