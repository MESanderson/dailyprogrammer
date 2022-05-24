module Main where

import System.Exit
import System.Random
import Data.Sort


retrieve_letter_or_underscore :: Char -> Char -> Char 
retrieve_letter_or_underscore a b
  | a == b = a
  | otherwise = '_'


produce_guess_result :: String -> String -> String
produce_guess_result actual guess = [retrieve_letter_or_underscore a b | (a, b) <- zip actual guess]

guess_match_count :: String -> String -> Int
guess_match_count actual guess = length [a | (a, b) <- zip actual guess, a == b]

guess actual guessval =
  guess_match_count actual guessval == length actual
  --produce_guess_result actual guessval == actual


handle_guess actual "q!" = do exitFailure
handle_guess actual guess
 | actual == guess = do return True
 | otherwise = do return  False
 


play_game actual words numguessesleft = do
  mapM_ print words
  let guess_prompt = "Enter guess " ++ "[" ++ show numguessesleft ++ "]: "
  putStr guess_prompt

  guess_val <- getLine

  correct_guess <- handle_guess actual guess_val

  if correct_guess then
    do putStrLn "You Win!"
  else if numguessesleft > 0 then do
       putStrLn $ produce_guess_result actual guess_val
       play_game actual words $ numguessesleft -1
  else
    do putStrLn "You Lose(er)"



get_words word_path word_size num_wrds = do
  contents <- readFile word_path
  g <- getStdGen
  let words_of_size = [x | x <- map init $ lines contents, (length x) == word_size]
  
  let actual_word = words_of_size !! head (randomRs(0, length words_of_size) g)
  
  --putStrLn $ "Found word: " ++ actual_word

  let g_filter = guess_match_count actual_word
  let similar_words = [x | x <- words_of_size,
                          g_filter x > 2,
                          g_filter x <= length actual_word,
                          x /= actual_word]

  let chosen_words = actual_word : take (num_wrds - 1) similar_words
  
  let word_list = sortOn fst $ zip (randomRs (0, length chosen_words) g) chosen_words
                         

  return (actual_word, map snd word_list)

main = do
  putStrLn "welcome"
  putStrLn "Select word length: "
  wrd_len_str <- getLine
  let wrd_len = read wrd_len_str :: Int

  putStr "Select num words: "
  num_wrds_str <- getLine
  let num_wrds = read num_wrds_str :: Int

  (actual_word, words_list) <- get_words "enable1.txt" wrd_len num_wrds

  play_game actual_word  words_list 8



  

