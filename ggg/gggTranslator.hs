module Main where

import qualified Data.Map as Map
import qualified Data.Tuple as Tuple
import Data.Char
import Data.List

tst_hdr = "H GgG d gGg e ggG l GGg o gGG r Ggg w ggg"

tst_msg = "GgGggGGGgGGggGG, ggggGGGggGGggGg!"
tst_rev_msg = "Hello, world!"

ggg_list_tokenizer (a:b:xs) = [(a, b)] ++ ggg_list_tokenizer xs
ggg_list_tokenizer [] = []


letter_to_ggg_map hdr_str = Map.fromList $ ggg_list_tokenizer $ words hdr_str
ggg_to_letter_map hdr_str = Map.fromList $ map Tuple.swap $ ggg_list_tokenizer $ words hdr_str


g_chunker str = groupBy chunkP str
   where chunkP = (\ x y -> (toLower x == 'g') == (toLower y == 'g'))


translate_g_chunk _ "" = ""
translate_g_chunk trnsMap str
  | startsWithG = Map.findWithDefault "***" (take 3 str) trnsMap ++ translate_g_chunk trnsMap (drop 3 str)
  | otherwise = str
  where startsWithG = 'g' == (toLower $ head str)


g_rev_translater str =
  concatMap (translate_g_chunk (ggg_to_letter_map tst_hdr)) chunked
  where chunked = g_chunker str


g_translater :: Map.Map [Char] [Char] -> [Char] -> [Char]
g_translater trnsMap "" = ""
g_translater trnsMap str
  | is_trns = Map.findWithDefault "_" first_ltr trnsMap ++ g_translater trnsMap (tail str)
  | otherwise = first_ltr ++ g_translater trnsMap  (tail str)
  where first_ltr = take 1 str
        is_trns = Map.member first_ltr trnsMap 



main = do
  putStrLn "OH HI"

  
