module Lib where

import Data.Text as T 
import Data.Text.IO as T ( readFile )
import Data.Char (toUpper, isLetter, toLower)
import System.Random
import Data.String (String)
import Data.Bool (Bool)
import Data.List (elemIndex)

--read list of words from words.txt
wordsList :: IO [Text]
wordsList = fmap T.lines(T.readFile "words.txt")

--get a random word from the list
randWord :: IO [Text] -> IO Text
randWord list = do
    l <- list
    index <- randomRIO (0, Prelude.length l - 1)
    return (l !! index)

--String to represent keyboard and available characters
keyBoard :: String
keyBoard = "q w e r t y u i o p\n a s d f g h j k l\n  z x c v b n m"

--Remove or capitalize letters based on guess and answer word
updateKeyboard :: String -> String -> String -> IO String
updateKeyboard [] _ keyBoard = return keyBoard
updateKeyboard (g : gs) ans keyBoard =
    --compare letters from guess to answer and eliminate or capitalize letters 
    case (g `Prelude.elem` ans) of
        True  -> updateKeyboard gs ans $ 
                    fmap (\x -> 
                    if x == g then Data.Char.toUpper g
                    else x) 
                    keyBoard
        False -> updateKeyboard gs ans $ 
                    fmap (\x -> 
                    if x == g then ' '
                    else x) 
                    keyBoard

--create tuple list which stores char and a bool if it is in correct position for each char
findCorrectChars :: String -> String -> [(Char, Bool)]
findCorrectChars guess ans = Prelude.zip guess $ Prelude.zipWith (==) ans guess 

--returns a string that shows letters in the correct position
outputString :: [(Char, Bool)] -> String
outputString []      = ""
outputString (h : t) = 
    if inPos then
        Data.Char.toUpper ch : ' ' : outputString t 
    else '_' : ' ' : outputString t
    where (ch, inPos) = h

--create a list of characters that exist in the word (capitalized letters in the keyboard)
lettersInWord :: String -> String
lettersInWord [] = ""
lettersInWord (k : ks) = 
    if Data.Char.isLetter k then
        if k == Data.Char.toUpper k then
            k : lettersInWord ks
        else lettersInWord ks
    else
        lettersInWord ks

--given a list of possible words and a string of known letters, returns a list of 
--words that match known letters
possibleWords :: [String] -> String -> [String]
possibleWords list letters = 
    Prelude.concatMap (\x -> if lettersMatch x letters then [x] else []) list

--Choose a random word from a list of potential words
compGuess :: [String] -> IO String
compGuess matches = do
    index <- randomRIO (0, (Prelude.length matches - 1)) 
    return $ matches !! index

--choose a word from known letters and their positions
compTurn :: String -> String -> IO String
compTurn key outp = do
    w <- wordsList 
    let wl = fmap unpack w 
    let p = goodGuess outp key wl
    if p == [] then
        return ""
    else
        compGuess p

--given a word and a string of known letters, check if word has those letters
lettersMatch :: String -> String -> Bool
lettersMatch word letters = 
    let search = 
            Prelude.concatMap (\x -> if (Data.Char.toLower x) `Prelude.elem` word then [x] else []) letters in
            if Prelude.length search == Prelude.length letters then
                True
            else
                False

--given a word, a string of a correct letter pattern, return if word has that pattern
matchWord :: String -> String -> Bool
matchWord [] [] = True
matchWord (w : ws) (l : ls) = 
    --ignore underscore characters from output str
    if Data.Char.isLetter l then
        if w == Data.Char.toLower l then
            matchWord ws ls
        else 
            False
    else 
        matchWord ws ls

--create a list of words that have known letters and letters in the correct position
goodGuess :: String -> String -> [String] -> [String]
goodGuess inPos keyb list =   
    let -- sanitize string of spaces
        words1 = Prelude.concatMap (\x -> if x == ' ' then [] else [x]) inPos
        --create list of words from list of all words and match words with letters in the correct position
        words2 = Prelude.concatMap (\x -> if (matchWord x words1) then [x] else []) list in
        --from the list of pattern matched words, get words that have appropriate letters
        Prelude.concatMap (\x -> if hasCorrectLetters keyb x && noIncorrectLetters keyb x then
                                    [x]
                                else
                                    [] ) words2
    
--check if word has all correct letters (regardless of position)
hasCorrectLetters :: String -> String -> Bool
hasCorrectLetters [] _ = True --empty list past in means all letters matched
hasCorrectLetters (k : ks) word = 
    --compare letters from keyboard and match known letters
    if Data.Char.isLetter k && k == Data.Char.toUpper k then
        if (Data.Char.toLower k) `Prelude.elem` word then
            hasCorrectLetters ks word
        else
            False
    else
        hasCorrectLetters ks word

--check if a word has letters that were ruled out
noIncorrectLetters :: String -> String -> Bool
noIncorrectLetters _ [] = True
noIncorrectLetters key (w:ws) = 
    --if a letter in the word exists in the keyboard
    if w `Prelude.elem` key || Data.Char.toUpper w `Prelude.elem` key then
        noIncorrectLetters key ws
    else
        False