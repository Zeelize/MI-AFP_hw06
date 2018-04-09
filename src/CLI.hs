module CLI (loadAndShow, testerCLI) where

import System.IO
import System.Environment
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as TL
import Data.Aeson (decode)
import Data.Maybe

import Tester.Model
import Tester.Answering
import Tester.Evaluation
import Tester.Model.AesonInstances


-- demonstration of using aeson to load JSON file with TestSet
loadAndShow :: IO ()
loadAndShow = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contentsJSON <- BS.hGetContents handle
  let contents = decode contentsJSON :: Maybe TestSet
  case contents of
    Nothing -> error "Invalid JSON..."
    Just c -> testProceed c

testProceed :: TestSet -> IO ()
testProceed (TestSet n i q) = do
  putStrLn $ "Welcome to " ++ TL.unpack n ++ "!"
  case i of
    Just intro -> putStrLn $ TL.unpack intro
    _ -> putStrLn "Show me your knowledge young padavan!"
  questionProceed q []

questionProceed :: [Question] -> [AnswerResult] -> IO ()
questionProceed ((Question t a):xs) r = do
  putStrLn questionSeparator
  putStrLn $ TL.unpack t
  putStrLn $ promptAnswer a
  inp <- getLine
  let uAnswer = readAnswer inp a
  let aRes = evaluate a uAnswer
  questionProceed xs (r ++ [AnswerResult {arAns = a, arRes = aRes}]) 

-- Evaluate if there is no questions left
questionProceed [] r = do
  putStrLn questionSeparator
  putStrLn questionSeparator
  resultsProceed r 0 0

resultsProceed :: [AnswerResult] -> Points -> Points -> IO ()
resultsProceed ((AnswerResult a (Result s m c)):xs) ts tm = do
  case isCorrect $ Result s m c of
    True -> putStrLn "You answered correctly!"
    _ -> putStrLn $ rightAnswer a
  resultsProceed xs (ts + s) (tm + m)

-- Print score/max score
resultsProceed [] ts tm = do
  putStrLn questionSeparator
  putStrLn $ "Your score is " ++ show ts ++ "/" ++ show tm ++ "."
    
testerCLI :: IO ()
testerCLI = loadAndShow
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
