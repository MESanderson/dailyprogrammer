module Main where


retrieve_letter_or_underscore :: Char -> Char -> Char 
retrieve_letter_or_underscore a b
  | a == b = a
  | otherwise = '_'


produce_guess_result :: String -> String -> String
produce_guess_result actual guess = [retrieve_letter_or_underscore a b | (a, b) <- zip actual guess]

guess actual guessval = do
  let guess_out = produce_guess_result actual guessval
  putStrLn(guess_out)
  return $ guess_out == actual

play_game actual words numguessesleft = do
  mapM_ print words
  let guess_prompt = "Enter guess " ++ "[" ++ show numguessesleft ++ "]: "
  putStr guess_prompt

  guess_val <- getLine
  correct_guess <- guess actual guess_val

  if correct_guess then
    do putStrLn "You Win!"
  else if numguessesleft > 0 then
    do putStrLn ""
       play_game actual words $ numguessesleft -1
  else
    do putStrLn "You Lose(er)"

get_words word_path word_size num_words = do
  contents <- readFile word_path
  return (take num_words [init x | x <- lines contents, (length x) == word_size])

main = do
  putStrLn "welcome"
  putStrLn "Select word length: "
  wrd_len_str <- getLine
  let wrd_len = read wrd_len_str :: Int

  putStr "Select num words: "
  num_wrds_str <- getLine
  let num_wrds = read num_wrds_str :: Int

  putStr "Word file path: "
  f_path <- getLine

  putStrLn f_path
  words_list <- get_words f_path wrd_len num_wrds

  play_game (head words_list) words_list 8


  

