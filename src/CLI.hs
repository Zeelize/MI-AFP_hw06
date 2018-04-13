{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : CLI
Description : Command Line Interface for selftester
Copyright   : (c) Marek Suchánek, 2018
                  Vojtěch Mráz, 2018
License     : MIT
Maintainer  : mrazvoj1@fit.cvut.cz
Stability   : experimental
Portability : POSIX

CLI for selftester. Test yourself with test written in JSON. 
To see help, run:
stack exec -- selftester -h
-}
module CLI (loadAndShow, testerCLI) where

import System.IO
import System.Environment
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as TL
import Data.Aeson (decode)
import Data.Maybe
import Turtle

import Tester.Model
import Tester.Answering
import Tester.Evaluation
import Tester.Helpers
import Tester.Model.AesonInstances

-- options parser
optParser :: Parser (Bool, Bool, Text, Maybe Text)
optParser = (,,,) <$> switch "shuffle" 's' "Shuffle randomly questions"
             <*> switch  "answer"  'a' "Give correct answer immediately"
             <*> argText  "src" "Path to json file"
             <*> optional (argText  "mode" "Run test in specific mode [train/learn]")

-- demonstration of using aeson to load JSON file with TestSet
loadAndShow :: String -> String -> Bool -> Bool -> IO ()
loadAndShow json flag s a = do
  handle <- openFile json ReadMode
  contentsJSON <- BS.hGetContents handle
  let contents = decode contentsJSON :: Maybe TestSet
  case contents of
    Nothing -> error "Invalid JSON file!"
    Just c -> testProceed c flag s a

testProceed :: TestSet -> String -> Bool -> Bool -> IO ()
testProceed (TestSet n i q) f s a = do
  putStrLn $ "Welcome to " ++ TL.unpack n ++ "!"
  case i of
    Just intro -> putStrLn $ TL.unpack intro
    _ -> putStrLn "Show me your knowledge young padavan!"
  proceedNext f a
  where 
    qq = case s of
      True -> shuffleQuestions q
      _ -> q
    
    proceedNext :: String -> Bool -> IO ()
    proceedNext f a 
      | f == "learn" = questionLearnProceed qq
      | f == "train" || a = questionTestProceed qq [] True
      | otherwise = questionTestProceed qq [] False

questionLearnProceed :: [Question] -> IO ()
questionLearnProceed ((Question t a):xs) = do
  putStrLn questionSeparator
  putStrLn $ TL.unpack t
  putStrLn $ rightAnswer a
  questionLearnProceed xs

questionLearnProceed [] = putStrLn "Hope you learned something!"

questionTestProceed :: [Question] -> [AnswerResult] -> Bool -> IO ()
questionTestProceed ((Question t a):xs) r f = do
  putStrLn questionSeparator
  putStrLn $ TL.unpack t
  putStrLn $ promptAnswer a
  inp <- getLine
  let uAnswer = readAnswer inp a
  let aRes = evaluate a uAnswer
  case f of
    True -> putStrLn $ rightAnswer a
    _ -> putStr "" 
  questionTestProceed xs (r ++ [AnswerResult {arAns = a, arRes = aRes}]) f
  
-- Evaluate if there is no questions left
questionTestProceed [] r _ = do
  putStrLn questionSeparator
  putStrLn questionSeparator
  resultsProceed r 0 0

resultsProceed :: [AnswerResult] -> Points -> Points -> IO ()
resultsProceed ((AnswerResult a (Result s m c)):xs) ts tm = do
  case isCorrect $ Result s m c of
    True -> putStrLn $ "-> You answered correctly: " ++ show s ++ " points!" 
    _ -> putStrLn $ rightAnswer a
  resultsProceed xs (ts + s) (tm + m)

-- Print score/max score
resultsProceed [] ts tm = do
  putStrLn questionSeparator
  putStrLn $ "Your score is " ++ show ts ++ "/" ++ show tm ++ "."
    
testerCLI :: IO ()
testerCLI = do
  (s, a, src, mode) <- options "" optParser
  loadAndShow (checkJsonFile src) (checkMode mode) s a
  -- TODO: make CLI for answering loaded Test
  --       stack exec selftester JSON_FILE [cmd]
  --       - in args there will be one .json file with test, load the TestSet
  --       - print the name of TestSet
  --       - start giving questions and receiving answers
  --       - print summary with correct/wrong answers and total/max score
  --
  --       - if [cmd]=="learn", dont ask but show answer immediately
  --       - else if [cmd]=="train", show the correct answer immediately after getting answer
  --       - else if no [cmd] is given, show question, get answer and print results at the end
  --       - else print some error message, same if file is not provided
  --
  --       Be creative, design it as you want and as you would like to use it!
  --       If you want to try cmdargs or other, feel free to do it! (you can have nice --version and --help)

-- TODO: try to deal here just with IO, dealing with "pure" Strings should be in different modules
