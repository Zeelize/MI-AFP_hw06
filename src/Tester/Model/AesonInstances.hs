{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : AesonInstances
Description : Command Line Interface for selftester
Copyright   : (c) Marek Suchánek, 2018
License     : MIT
Maintainer  : mrazvoj1@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Specified AesonInstances for loading tests written in json file.
-}
module Tester.Model.AesonInstances where

import Tester.Model
import Data.Aeson
import Data.Aeson.Types
import Data.Text.Lazy


-- | Parse answer choice
instance FromJSON Choice where
  parseJSON (Object o) = Choice <$>
                          o .: "text"  <*>
                          o .: "score"
  parseJSON _ = fail "Expected an object"

-- | Parse question Answer
instance FromJSON Answer where
  parseJSON = withObject "Answer" $ \o -> do
    kind <- o .: "kind"
    case kind of
      "singlechoice" -> SingleChoice  <$> o .: "choices"
      "multichoice"  -> MultiChoice   <$> o .: "choices"
      "textual"      -> TextualAnswer <$> o .: "corrects" <*> o .: "score"
      "numeric"      -> NumericAnswer <$> o .: "corrects" <*> o .: "score"
      _              -> fail ("Unknown Answer kind: " ++ kind)

-- | Parse test Question
instance FromJSON Question where
  parseJSON (Object o) = Question         <$>
                          o .:  "text"    <*>
                          o .:  "answer"
  parseJSON _ = fail "Expected an object"

-- | Parse whole Test instance
instance FromJSON TestSet where
  parseJSON (Object o) = TestSet <$>
                          o .:  "title" <*>
                          o .:? "intro" <*>
                          o .:  "questions"
  parseJSON _ = fail "Expected an object"
