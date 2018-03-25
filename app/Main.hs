module Main where

import Art
import Hangman
import Data.Char (toLower)

main :: IO ()
main = do
  wrd <- randomWord'
  let puzzle = freshPuzzle $ fmap toLower wrd
  clear
  drawTitle
  runGame puzzle
