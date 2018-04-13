{-|
Module      : Evalutation
Description : Module for evaluating user answers and final scores
Copyright   : (c) Marek Suchánek, 2018
                  Vojtěch Mráz, 2018
License     : MIT
Maintainer  : mrazvoj1@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Module for evaluating user answers, results and calculating max and user score of the question or whole test. 
-}
module Tester.Evaluation where

import Tester.Model

-- | The 'isCorrect' function returns if the user result is for one answer is correct or not.
-- It takes one argument, of type 'Result'.
isCorrect :: Result -> Bool
isCorrect Result { reCorrectness = Total } = True
isCorrect _ = False

-- | Class which contains computation of max score, user score and evaluation of user input.
class Scored a where
  -- | 'maxScore' computes max possible points for instance 'a'.
  maxScore :: a -> Points
  -- | 'score' computes user score for instance 'a'.
  score :: a -> UserAnswer -> Points
  -- | 'evaluate' computes result/s for instance 'a' of user answers.
  evaluate :: a -> UserAnswer -> Result
  evaluate x ua = Result { rePoints    = points
                         , reMaxPoints = maxPoints
                         , reCorrectness = correctnessOf points maxPoints
                         }
              where points = score x ua
                    maxPoints = maxScore x
                    correctnessOf x y | x == y    = Total
                                      | x <= 0    = None
                                      | otherwise = Partial

-- | Proceed counting maxScore and user score for whole test set
instance Scored TestSet where
  maxScore = sum . map maxScore . tsItems
  score ts (ListOfUA uas) = sum . map (uncurry score) $ zip (tsItems ts) uas
  score _ _ = error "Expecting list of answers for whole TestSet"

-- | Proceed counting maxScore and user score for one question
instance Scored Question where
  maxScore = maxScore . quAnswer
  score = score . quAnswer

-- | Proceed counting maxScore and user score for question answer
instance Scored Answer where
  maxScore SingleChoice{ ascChoices = chs } = maximum . map chScore $ chs
  maxScore MultiChoice{ amcChoices = chs } = sum . filter (>0) . map chScore $ chs
  maxScore TextualAnswer{ ataScore = sc } = sc
  maxScore NumericAnswer{ anaScore = sc } = sc
  score SingleChoice{ ascChoices = chs } (ChoicesUA [x]) =
        case filter (\y -> chText y == x) chs of
          [ch] -> chScore ch
          _    -> 0
  score MultiChoice{ amcChoices = chs } (ChoicesUA xs) =
        case filter (\x -> chText x `elem` xs) chs of
          []   -> 0
          cchs -> sum . map chScore $ cchs
  score TextualAnswer{ ataCorrect = corrects, ataScore = sc } (TextualUA txt)
        | txt `elem` corrects = sc
        | otherwise = 0
  score NumericAnswer{ anaCorrect = corrects, anaScore = sc } (NumericUA val)
        | val `elem` corrects = sc
        | otherwise = 0
  score _ _ = error "Cannot determine the score"
