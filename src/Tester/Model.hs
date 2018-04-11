{-|
Module      : Model
Description : Model structure of selftester
Copyright   : (c) Marek Suchánek, 2018
                  Vojtěch Mráz, 2018
License     : MIT
Maintainer  : mrazvoj1@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Init model of test.
Every test is a TestSet with name, optional intro and set of questions.
Question contain text and answer.
Answer is either Single choice, multi choice, textual or numeral.
For evaluating, model is using useranswer and result data.
-}
module Tester.Model where

import Data.Text.Lazy

type Points = Int

data TestSet = TestSet { tsName  :: !Text
                       , tsIntro :: Maybe Text
                       , tsItems :: [Question]
                       } deriving (Read, Show)

data Question = Question { quText    :: !Text
                         , quAnswer  :: Answer
                         } deriving (Read, Show)

data Answer = SingleChoice  { ascChoices :: [Choice] }
            | MultiChoice   { amcChoices :: [Choice] }
            | TextualAnswer { ataCorrect :: [Text], ataScore :: Points }
            | NumericAnswer { anaCorrect :: [Double], anaScore :: Points }
            deriving (Read, Show)

data Choice = Choice { chText    :: !Text
                     , chScore   :: Points
                     } deriving (Read)

instance Show Choice where
    show (Choice t s) = unpack t ++ ": " ++ show s ++ " point/s."

data UserAnswer = ChoicesUA [Text]
               | TextualUA !Text
               | NumericUA Double
               | ListOfUA [UserAnswer]
               deriving (Read, Show)

data Result = Result { rePoints    :: Points
                     , reMaxPoints :: Points
                     , reCorrectness :: Correctness
                     } deriving (Read, Show)

data Correctness = Total | Partial | None
                 deriving (Read, Show)

data AnswerResult = AnswerResult    { arAns :: Answer 
                                    , arRes :: Result 
                                    } deriving (Read, Show)
