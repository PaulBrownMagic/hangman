module Hangman where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import Art
--                   word   discovered   guessed
data Puzzle = Puzzle String [Maybe Char] [Char]
type WordList = [String]

instance Show Puzzle where
    show (Puzzle word discovered guessed) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ " Also guessed so far: " ++ misses
        ++ " (" ++ (show $ length misses) ++ "/" ++ (show maxGuesses)
        ++ " letters)"
        where
            misses = filter (notInWord word) guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

maxGuesses :: Int
maxGuesses = 9

allWords :: IO WordList
allWords = do
    dict <- readFile "/usr/share/dict/british-english"
    return (lines dict)

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)
    where gameLength w = length w >= minWordLength && length w < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
    let upperBound = length wl - 1
    randomIndex <- randomRIO (0, upperBound)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s d []
                where d = map nowt s
                      nowt = const Nothing

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = elem c word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

numberOfMisses :: Puzzle -> Int
numberOfMisses (Puzzle word _ guessed) = length $ filter (notInWord word) guessed

notInWord :: String -> Char -> Bool
notInWord wd l = not $ elem l wd

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledIn (c : s)
                                                where zipper guessed wordChar guessChar =
                                                          if wordChar == guessed
                                                          then Just wordChar
                                                          else guessChar
                                                      newFilledIn = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that, guess again."
            return puzzle
        (True, _) -> do
            putStrLn $ "Correct! " ++ [guess] ++ " is in the word."
            return $ fillInCharacter puzzle guess
        (False, _) -> do
            putStrLn $ guess : " was not in the word."
            return $ fillInCharacter puzzle guess

clear :: IO ()
clear = putStr "\ESC[2J"

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle word _ guessed) = if numberOfMisses puzzle == maxGuesses
                                          then do
                                              drawImg puzzle
                                              putStrLn "You Lose!"
                                              putStrLn $ "The word was: \"" ++ word ++ "\""
                                              exitSuccess
                                          else return ()

gameWin :: Puzzle -> IO ()
gameWin puzzle@(Puzzle word known _) = if all isJust known
                                then do
                                    drawImg puzzle
                                    putStrLn "You win!"
                                    putStrLn $ "The word was: " ++ word
                                    exitSuccess
                                else return ()

drawImg :: Puzzle -> IO ()
drawImg puzzle = do
    drawHangman (numberOfMisses puzzle)

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameWin puzzle
    gameOver puzzle
    drawImg puzzle
    putStrLn $ show puzzle
    putStrLn "Guess a letter: "
    guess <- getLine
    clear
    drawTitle
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ -> putStrLn "Guess must be a single character."

