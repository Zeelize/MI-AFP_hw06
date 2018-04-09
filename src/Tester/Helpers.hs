module Tester.Helpers where

import Tester.Model
import System.FilePath

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
