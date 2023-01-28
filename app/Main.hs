module Main where

import Lib 
import Data.Text
import Data.Char
import Control.Monad (when)
import System.Exit (exitSuccess)
import Data.String (String)

main :: IO ()
main = do
    putStrLn greeting
    wd <- randWord wordsList
    gameLoop 6 keyBoard wd 

--game loop control
gameLoop :: Int -> String -> Text -> IO ()
gameLoop tries keyB word = do
    gameLose tries (unpack word)
    putStrLn $ "\n" ++ keyB
    putStrLn $ "\n" ++ (show tries) ++ " guesses remaining" 
    guess <- getGuess "Guess a 5 letter word"
    gameWin guess (unpack word)
    newKeyb <- updateKeyboard guess (unpack word) keyB
    putStrLn "\n"
    let outp = outputString $ findCorrectChars guess (unpack word)
    putStrLn $ "Your guess is: \n" ++ (spaceWord guess)
    putStrLn outp
    cpu <- compTurn newKeyb outp 
    putStrLn $ "\nThe computer guesses: \n" ++ (spaceWord cpu)
    putStrLn $ outputString $ findCorrectChars cpu (unpack word)
    compWin cpu (unpack word)
    newKey <- updateKeyboard cpu (unpack word) newKeyb 
    if tries == 0 then
        putStrLn "Game Over"
    else
        gameLoop (tries-1) newKey word 

--add spaces inbetween characters of a string
spaceWord :: String -> String
spaceWord [] = ""
spaceWord (w : ws) = w : ' ' : spaceWord ws 

--Greeting string to print
greeting :: String
greeting = "\n****Welcome to Wordle****\n"
        ++ "Characters not shown are not in the word\n"
        ++ "Capitalized characters are in the word\n"
        ++ "Letters in the correct position are printed\n"

--Prompt to get a guess
getGuess :: String -> IO String
getGuess msg = do
    putStrLn msg
    gu <- Prelude.getLine
    let guess = fmap Data.Char.toLower gu
    valid <- inList guess
    case valid of
        ValidGuess -> return guess
        NotInList  -> getGuess "Word is not in list, guess again!"
        NotFive    -> getGuess "Word is not 5 letters, guess again!"

--Type to set the conditions of the guess
data GuessCondition = NotInList | NotFive | ValidGuess
    deriving (Eq, Show)

--check if guess is a word in the list of words
findInList :: Text -> [Text] -> Bool
findInList guess list = guess `Prelude.elem` list

--Check guess to see if its a valid word
inList :: String -> IO GuessCondition
inList guess = do
    list <- wordsList
    case (findInList (pack guess) list) of
        True  -> return ValidGuess
        False -> let len = Prelude.length guess in
                 if len == 5 then
                    return NotInList
                 else 
                    return NotFive

--check if guess is correct
gameWin :: String -> String -> IO ()
gameWin guess word = 
    when (guess == word) $ 
        do
            putStrLn $ "\nCongrats! You WON.\nThe word was " ++ (fmap Data.Char.toUpper guess)
            putStrLn $ "Play again? (y): "
            cmd <- getLine
            case cmd of
                "y" -> main
                _   -> exitSuccess

--check if computer guess is correct
compWin :: String -> String -> IO ()
compWin str  ans= 
    when (str == ans) $
        do
            putStrLn $ "\nThe Computer won.\nThe word was " ++ (fmap Data.Char.toUpper str)
            putStrLn $ "Play again? (y): "
            cmd <- getLine
            case cmd of
                "y" -> main
                _   -> exitSuccess

--check if all tries used up
gameLose :: Int -> String -> IO ()
gameLose try word = 
    when (try == 0) $
        do
            putStrLn $ "\nGame over\n" ++ "The word was " ++ (show word)
            putStrLn $ "Play again? (y): "
            cmd <- getLine
            case cmd of
                "y" -> main
                _   -> exitSuccess 