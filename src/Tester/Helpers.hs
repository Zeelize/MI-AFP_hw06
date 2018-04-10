module Tester.Helpers where

import Tester.Model
import System.FilePath (takeExtension)
import System.Random
import Data.List
import System.IO.Unsafe
import qualified Data.Text as T

checkJsonFile :: T.Text -> String
checkJsonFile t 
    | takeExtension (T.unpack t) == ".json" = T.unpack t
    | otherwise = error "First argument has to be .json file!"


checkMode :: Maybe T.Text -> String
checkMode Nothing = ""
checkMode (Just t) 
    | mode == "learn" || mode == "train" = mode
    | otherwise = error "Only learn and train modes are available!"
    where
        mode = T.unpack t 

shuffleQuestions :: [Question] -> [Question]
shuffleQuestions q = shuffleQ (mkStdGen userRandomNum) q
    where 
        userRandomNum :: Int
        userRandomNum = unsafePerformIO $ getStdRandom (randomR (1, 100))

shuffleQ :: StdGen -> [Question] -> [Question]
shuffleQ _ [] = []
shuffleQ gen xs = front : shuffleQ newGen (take n xs ++ drop (n + 1) xs)
    where 
        (n, newGen) = randomR (0, length xs -1) gen
        front = xs !! n
