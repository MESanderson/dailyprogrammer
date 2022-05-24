module Main where

import qualified Data.Map as Map
import qualified Data.Tuple as Tuple
import Data.Char
import Data.List
import qualified Data.Set as Set

tst_hdr = "H GgG d gGg e ggG l GGg o gGG r Ggg w ggg"

tst_msg = "GgGggGGGgGGggGG, ggggGGGggGGggGg!"
tst_rev_msg = "Hello, world!"

-- |chunk function to group list into list of n element lists
-- similar to simple version of clojure's partition
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk size list
  | size > 0 = (take size list) : chunk size (drop size list)
  | otherwise = []

-- Simple function to turn lists of size 2 to 2 element tuples
list_to_tup2 :: [a] -> (a, a)
list_to_tup2 [a,b] = (a,b)


alphaNumAll = [chr x | x <- [65..122], isAlphaNum $ chr x] ++ [intToDigit x | x <- [0..9]]

-- letter to ggg value map creator
letter_to_ggg_map hdr_str = Map.fromList $ map list_to_tup2 (chunk 2  $ words hdr_str)

-- ggg value to letter map creator
ggg_to_letter_map hdr_str = Map.fromList $ map Tuple.swap $
  map list_to_tup2 (chunk 2 $ words hdr_str)

-- |Function for grouping by contiguous values "G/g" or non "G/g"
-- example: g_chunker "gGggOhnomoreGs" = ["gGgg", "Ohnomore", "G", "s"]
g_chunker :: String -> [String]
g_chunker str = groupBy chunkP str
   where chunkP = (\ x y -> (toLower x == 'g') == (toLower y == 'g'))

-- |Function to decode a string of consecutive G/g values using a
-- Map of G/g keys to letter values
translate_g_chunk :: Map.Map [Char] [Char] -> [Char] -> [Char]
translate_g_chunk _ "" = ""
translate_g_chunk trnsMap str
  | startsWithG = Map.findWithDefault "*" (take gChunkSize str) trnsMap ++
                    translate_g_chunk trnsMap (drop gChunkSize str)
  | otherwise = str
  where startsWithG = 'g' == (toLower $ head str)
        gChunkSize = (length . fst . head. Map.toList) trnsMap


-- |Function to encode string broken into consecutive 'g/G' values or non 'g/G' values
-- example: ["GgGggG", ", ", "ggGGGg", "!"] = "Oh, Hi!"
g_rev_translater :: Map.Map [Char] [Char] -> [Char] -> [Char]
g_rev_translater gToCharMap str =
  concatMap (translate_g_chunk gToCharMap) chunked
  where chunked = g_chunker str


-- |Function to encode message to g/G message
g_encoder :: Map.Map [Char] [Char] -> [Char] -> [Char]
g_encoder charToGMap "" = ""
g_encoder charToGMap str
  | is_trns = Map.findWithDefault "_" first_ltr charToGMap ++
                g_encoder charToGMap (tail str)
  | otherwise = first_ltr ++ g_encoder charToGMap  (tail str)
  where first_ltr = take 1 str
        is_trns = Map.member first_ltr charToGMap 

-- |Function to turn integers into binary 'g/G' values (0='g', 1='G') string
intTogggIter :: Integer -> [Char]
intTogggIter i
  | i == 0 = ""
  | r == 1 = intTogggIter d ++ "G"
  | r == 0 = intTogggIter d ++ "g"
  where (d, r) = divMod i 2


-- |Function to produce a char to G/g Map based on unique characters
char_to_g_map_creator :: [Char] -> Map.Map [Char] [Char] 
char_to_g_map_creator msg =
  Map.fromList $ zip uniqueLettersString gggIter 
  where uniqueLetters = filter isAlphaNum $ Set.toList (Set.fromList msg)
        -- TODO: Insert some randomization
        letterOrdStart = (floor (logBase 2.0 (fromIntegral $  length uniqueLetters)) + 1) ^ 2
        uniqueLettersString = [[c] | c <- uniqueLetters]
        gggIter = [intTogggIter i | i <- [(toInteger letterOrdStart)..]]

        

exampleMsg =  "I did not! Oh hai mark."
exampleChToGMap = char_to_g_map_creator exampleMsg
exampleGToChMap = Map.fromList ([(b, a) | (a,b) <- Map.toList exampleChToGMap])
exampleGggEncode = g_encoder  exampleChToGMap exampleMsg
exampleRevEncode = g_rev_translater exampleGToChMap exampleGggEncode
exampleTrue = exampleMsg == exampleRevEncode


decodePrompt = do
  putStr "Please enter g/G key (<letter> <gcombo> ... format): "
  g_key <- getLine
  putStr "Please enter ggg message: "
  g_msg <- getLine

  let gToCharMap = ggg_to_letter_map g_key
  let decoded = g_rev_translater gToCharMap g_msg

  return (decoded)
  

encodePrompt = do
  putStr "Please enter message"
  msg <- getLine

  let charToGMap = char_to_g_map_creator msg
  let encoded = g_encoder charToGMap msg

  return (encoded)


main = do
  putStr "(D)ecode or (E)ncode?:"
  action <- getLine
  output <- case (toLower . head) action
        of 'd' -> decodePrompt
           'e' -> encodePrompt
--             _ -> do putStrLn "Please enter d for decode or e for encode" 

  putStrLn output
  
