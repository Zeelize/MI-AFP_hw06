module EvaluatingSpec (spec) where

import Test.Hspec    

import Tester.Evaluation
import qualified TestData.Models as TM
    
spec :: Spec
spec = do
    describe "Result and Scored class" $ do
        it "has right correctness of the result" $ do
            (isCorrect TM.res1) `shouldBe` True
            (isCorrect TM.res2) `shouldBe` False
            (isCorrect TM.res3) `shouldBe` False
        it "calculates maxScore" $ do
            maxScore TM.a2 `shouldBe` 2
            maxScore TM.q1 `shouldBe` 3
            maxScore TM.q3 `shouldBe` 1
            maxScore TM.test1 `shouldBe` 8
        it "calculates score" $ do
            score TM.a4 TM.ua3 `shouldBe` 2
            score TM.q2 TM.ua1 `shouldBe` 0
            score TM.q2 TM.ua4 `shouldBe` -1
            score TM.q1 TM.ua2 `shouldBe` 3
            score TM.q3 TM.ua4 `shouldBe` -1
            score TM.q4 TM.ua3 `shouldBe` 2
            score TM.test1 TM.ua5 `shouldBe` 4
        it "evaluates result" $ do
            show (evaluate TM.a4 TM.ua3) `shouldBe` show TM.res1
            show (evaluate TM.q2 TM.ua1) `shouldBe` show TM.res3
            show (evaluate TM.test1 TM.ua5) `shouldBe` show TM.res2
