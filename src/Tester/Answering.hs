{-|
Module      : Answering
Description : Module for reading and prompting answers
Copyright   : (c) Marek Suchánek, 2018
                  Vojtěch Mráz, 2018
License     : MIT
Maintainer  : mrazvoj1@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Help module with functions for reading answer, prompting answer choices and right answer.
-}
module Tester.Answering where

import Data.Maybe
import qualified Data.Text.Lazy as T

import Tester.Model


readAnswer :: String -> Answer -> UserAnswer
readAnswer str SingleChoice{ ascChoices = chs } = ChoicesUA uachs
           where uachs = map chText . catMaybes $ [lookup (read str) (zip [1..] chs)]
readAnswer str MultiChoice{ amcChoices = chs } = ChoicesUA uachs
           where uachs = map (chText . snd) . filter (\x -> fst x `elem` choices) $ zip [1..] chs
                 choices :: [Int]
                 choices = map read . words $ str
readAnswer str TextualAnswer{} = TextualUA . T.pack $ str
readAnswer str NumericAnswer{} = NumericUA . read   $ str

promptAnswer :: Answer -> String
promptAnswer SingleChoice{ ascChoices = chs } = choicesLines chs ++ "\nEnter your choice number (e.g. 1): "
promptAnswer MultiChoice{ amcChoices = chs } = choicesLines chs ++ "\nEnter your choices numbers (e.g. 1 3): "
promptAnswer TextualAnswer{} = "Enter your answer as text: "
promptAnswer NumericAnswer{} = "Enter your answer as number: "

choicesLines :: [Choice] -> String
choicesLines = unlines . zipWith choiceLine [1..] . map (T.unpack . chText)
              where
                choiceLine i c = show i ++ ":" ++ c

questionSeparator :: String
questionSeparator = replicate 50 '-'

-- TODO: finish with functions you need...
rightAnswer :: Answer -> String
rightAnswer SingleChoice{ ascChoices = chs } = "-> Correct answers:\n" ++ showChoices chs
rightAnswer MultiChoice{ amcChoices = chs } = "-> Correct answers:\n" ++ showChoices chs
rightAnswer TextualAnswer{ ataCorrect = ct } = "-> Correct answer/s: " ++ show ct
rightAnswer NumericAnswer{ anaCorrect = cn } = "-> Correct answer: " ++ show cn

showChoices :: [Choice] -> String
showChoices [] = ""
showChoices (x:xs) = show x ++ "\n" ++ showChoices xs
