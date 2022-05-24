module Main where

import qualified Data.Map as Map
import qualified Data.Tuple as Tuple
import Data.Char
import Data.List
import qualified Data.Set as Set

tst_hdr = "H GgG d gGg e ggG l GGg o gGG r Ggg w ggg"

tst_msg = "GgGggGGGgGGggGG, ggggGGGggGGggGg!"
tst_rev_msg = "Hello, world!"

ggg_list_tokenizer (a:b:xs) = [(a, b)] ++ ggg_list_tokenizer xs
ggg_list_tokenizer [] = []

alphaNumAll = [chr x | x <- [65..122], isAlphaNum $ chr x] ++ [intToDigit x | x <- [0..9]]



letter_to_ggg_map hdr_str = Map.fromList $ ggg_list_tokenizer $ words hdr_str
ggg_to_letter_map hdr_str = Map.fromList $ map Tuple.swap $ ggg_list_tokenizer $ words hdr_str


g_chunker str = groupBy chunkP str
   where chunkP = (\ x y -> (toLower x == 'g') == (toLower y == 'g'))


translate_g_chunk _ "" = ""
translate_g_chunk trnsMap str
  | startsWithG = Map.findWithDefault "*" (take gChunkSize str) trnsMap ++ translate_g_chunk trnsMap (drop gChunkSize str)
  | otherwise = str
  where startsWithG = 'g' == (toLower $ head str)
        gChunkSize = (length . fst . head. Map.toList) trnsMap


g_rev_translater gToCharMap str =
  concatMap (translate_g_chunk gToCharMap) chunked
  where chunked = g_chunker str


char_to_g_translater :: Map.Map [Char] [Char] -> [Char] -> [Char]
char_to_g_translater trnsMap "" = ""
char_to_g_translater trnsMap str
  | is_trns = Map.findWithDefault "_" first_ltr trnsMap ++ char_to_g_translater trnsMap (tail str)
  | otherwise = first_ltr ++ char_to_g_translater trnsMap  (tail str)
  where first_ltr = take 1 str
        is_trns = Map.member first_ltr trnsMap 


intTogggIter i
  | i == 0 = ""
  | r == 1 = intTogggIter d ++ "G"
  | r == 0 = intTogggIter d ++ "g"
  where (d, r) = divMod i 2


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
exampleGggEncode = char_to_g_translater exampleChToGMap exampleMsg
exampleRevEncode = g_rev_translater exampleGToChMap exampleGggEncode
exampleTrue = exampleMsg == exampleRevEncode


main = do
  putStrLn "OH HI"

  
