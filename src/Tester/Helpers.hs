module Tester.Helpers where

import Tester.Model
import System.FilePath
import System.Random.Shuffle
import System.Random
import Data.List

parseArgsJsonFile :: [String] -> String
parseArgsJsonFile (x:xs) 
    | takeExtension x == ".json" = x
    | otherwise = error "First argument has to be .json file!"


getLearnTrainArg :: [String] -> String
getLearnTrainArg [] = ""
getLearnTrainArg [mode] 
    | mode == "learn" || mode == "train" = mode
    | otherwise = error "Only learn and train modes are available!"
getLearnTrainArg _ = error "Wrong arguments!"

shuffleQuestions :: [Question] -> [Question]
shuffleQuestions q = shuffleQ (mkStdGen 666) q

shuffleQ :: StdGen -> [Question] -> [Question]
shuffleQ _ [] = []
shuffleQ gen xs = front : shuffleQ newGen (take n xs ++ drop (n + 1) xs)
    where 
        (n, newGen) = randomR (0, length xs -1) gen
        front = xs !! n
