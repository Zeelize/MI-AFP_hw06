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

-- | Represents number of points, full numbers int.
type Points = Int

-- | Represents whole testSet
data TestSet = TestSet { tsName  :: !Text -- ^ 'tsName' is the name of the test
                       , tsIntro :: Maybe Text -- ^ 'tsIntro' is optional introduction to a test
                       , tsItems :: [Question] -- ^ 'tsItems' is the list of all questions
                       } deriving (Read, Show)

-- | Represents one test question
data Question = Question { quText    :: !Text -- ^ 'quText' is the text of the question
                         , quAnswer  :: Answer -- ^ 'quAnswer' is the correct answer for the question
                         } deriving (Read, Show)

-- | Represents correct answer for the question
data Answer =
            -- | Answer is a single choice with list of all choices
            SingleChoice  { ascChoices :: [Choice] }
            -- | Answer is a multiple choice with list of all choices
            | MultiChoice   { amcChoices :: [Choice] }
            -- | Answer is textual with list of acceptable answers and possible points
            | TextualAnswer { ataCorrect :: [Text], ataScore :: Points }
            -- | Answer is numeric with list of acceptable answers and possible points
            | NumericAnswer { anaCorrect :: [Double], anaScore :: Points }
            deriving (Read, Show)

-- | Represents one choice in choice-type questions
data Choice = Choice { chText    :: !Text -- ^ 'chText' is the text of the choice
                     , chScore   :: Points -- ^ 'chScore' is the number of points you get if you chose this choice
                     } deriving (Read)

-- | Show choice in readable format
instance Show Choice where
    show (Choice t s) = unpack t ++ ": " ++ show s ++ " point/s."

-- | Represents user answer for specific question.
data UserAnswer = 
                -- | List of picked choices for single and multiples choice questions
                ChoicesUA [Text]
                -- | Actual user answer for textual question
                | TextualUA !Text
                -- | Actual user answer for numeric question
                | NumericUA Double
                -- | List of all userAnswers
                | ListOfUA [UserAnswer]
                deriving (Read, Show)

-- | Represents user result of user answer with points and correctnes of the answer.
data Result = Result { rePoints    :: Points -- ^ 'rePoints' is the points for this result
                     , reMaxPoints :: Points -- ^ 'reMaxPoints' is the max number of points you could get
                     , reCorrectness :: Correctness -- ^ 'reCorrectness' is the correstnest of the result
                     } deriving (Read, Show)

-- | Represents correctnes of the result
data Correctness =  
                    -- | User answer is completely correct
                    Total 
                    -- | User answer is not completely correct
                    | Partial 
                    -- | User answer is wrong
                    | None
                 deriving (Read, Show)

-- | Connects right answer and user results
data AnswerResult = AnswerResult    { arAns :: Answer -- ^ 'arAns' is the correct answer
                                    , arRes :: Result -- ^ 'arRes' is the user answer
                                    } deriving (Read, Show)
