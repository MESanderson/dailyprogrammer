module Main where

import qualified Data.Map as Map
import qualified Data.Tuple as Tuple
import Data.Char
import Data.List
import qualified Data.Set as Set


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

gCodeMatch gCode str = take (length gCode) str == gCode


-- |Function to decode a ggg encoded string using a
-- Map of G/g keys to letter values
g_decoder :: Map.Map [Char] [Char] -> [Char] -> [Char]
g_decoder _ [] = []
g_decoder gToCharMap str 
  | gMatches == [] = (head str) : (nextTrans (tail str))
  | otherwise = (snd . head) gMatches ++ nextTrans (drop ((length . fst . head) gMatches) str)
  where gListSorted = Map.toAscList gToCharMap
        gMatches = filter (\(a,b) -> gCodeMatch a str) gListSorted
        nextTrans = g_decoder gToCharMap


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
exampleRevEncode = g_decoder exampleGToChMap exampleGggEncode
exampleTrue = exampleMsg == exampleRevEncode


decodePrompt = do
  putStr "Please enter g/G key (<letter> <gcombo> ... format): "
  g_key <- getLine
  putStr "Please enter ggg message: "
  g_msg <- getLine

  let gToCharMap = ggg_to_letter_map g_key
  let decoded = g_decoder gToCharMap g_msg

  return (decoded)
  

encodePrompt = do
  putStr "Please enter message: "
  msg <- getLine

  let charToGMap = char_to_g_map_creator msg
  let encoded = g_encoder charToGMap msg

  let key_out = concatMap (\(a,b) -> a ++ " " ++ b ++ " ") $ Map.toList charToGMap
  
  return ("Key:\n" ++ key_out ++ "\n\nMessage:\n" ++ encoded)


main = do
  putStr "(D)ecode or (E)ncode?:"
  action <- getLine
  output <- case (toLower . head) action
        of 'd' -> decodePrompt
           'e' -> encodePrompt
--             _ -> do putStrLn "Please enter d for decode or e for encode" 

  putStrLn output
  
