# wordle

Peter Nguyen

This project was built using stack v. 2.7.5 with the default template and GHC v. 8.10.7.
This program was tested using stack and can be run/built with: "stack build" and/or "stack run".

ABOUT THE GAME:
This is a Wordle type game where the user has 6 guesses to guess a 5 letter word but with a twist.
A keyboard layout is printed before each guess showing the available letters where if the letter is in the word, then it is
capitalized. If the letter is not in the word then it is not printed on the keyboard layout.
After each guess, an output is given showing the pattern of the word based on the guess word which only prints out the letter
if it is in the correct position, else it prints '_' in the letters place.
The twist is the player will be playing a computer which makes a guess based on all the information also available to the player. 
It looks for words with a matching character layout and also reduces that list of words based on which letters are in the word 
and their respective positions.

BE WARNED, the txt file with the list of words the game draws from has strange words that may not be familar.