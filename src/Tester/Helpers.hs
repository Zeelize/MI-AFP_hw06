{-|
Module      : Helpers
Description : Module for help functions used in CLI
Copyright   : (c) Vojtěch Mráz, 2018
License     : MIT
Maintainer  : mrazvoj1@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Help module with function for checking if SRC is json file, checking right run mode [learn/train] and random shuffle of questions.
-}
module Tester.Helpers where

import Tester.Model
import System.FilePath (takeExtension)
import System.Random
import Data.List
import System.IO.Unsafe
import qualified Data.Text as T

-- | The 'checkJsonFile' function check if the file in arguments ends with .json extension.
-- It takes one argument, of type 'Text', file path.
checkJsonFile :: T.Text -> String
checkJsonFile t 
    | takeExtension (T.unpack t) == ".json" = T.unpack t
    | otherwise = error "First argument has to be .json file!"


-- | The 'checkMode' function check second argument if its valid running mode.
-- It takes one argument, of type 'Text', mode argument.
checkMode :: Maybe T.Text -> String
checkMode Nothing = ""
checkMode (Just t) 
    | mode == "learn" || mode == "train" = mode
    | otherwise = error "Only learn and train modes are available!"
    where
        mode = T.unpack t 

-- | The 'shuffleQuestions' function randomly shuffles the list of questions.
-- It takes one argument, of type '[Question]'.
shuffleQuestions :: [Question] -> [Question]
shuffleQuestions q = shuffleQ (mkStdGen userRandomNum) q
    where 
        -- | The 'userRandom' function generate new random generator and get random number from that.
        userRandomNum :: Int
        userRandomNum = unsafePerformIO $ getStdRandom (randomR (1, 100))
        
        -- | The 'shuffleQ' function shuffles the list of questions with random generator.
        -- It takes two arguments, of type 'StdGen' and '[Question]'.
        shuffleQ :: StdGen -> [Question] -> [Question]
        shuffleQ _ [] = []
        shuffleQ gen xs = front : shuffleQ newGen (take n xs ++ drop (n + 1) xs)
            where 
                (n, newGen) = randomR (0, length xs -1) gen
                front = xs !! n

