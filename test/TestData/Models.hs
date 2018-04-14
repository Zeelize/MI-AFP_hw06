module TestData.Models where

import qualified Data.Text.Lazy as T

import Tester.Model

cor1 = Total
cor2 = Partial
cor3 = None

res1 = Result   { rePoints    = 2
                , reMaxPoints = 2
                , reCorrectness = cor1
                }

res2 = Result   { rePoints    = 4
                , reMaxPoints = 8
                , reCorrectness = cor2
                }

res3 = Result   { rePoints    = 0
                , reMaxPoints = 2
                , reCorrectness = cor3
                }

ua1 = ChoicesUA [T.pack "hello", T.pack "cus"]
ua2 = TextualUA . T.pack $ "Prelude"
ua3 = NumericUA 20.0
ua4 = ChoicesUA [T.pack "hello"]
ua5 = ListOfUA [ua1, ua2, ua3, ua4]

ch1 = Choice    { chText    = T.pack "ahoj"
                , chScore   = 1
                }

ch2 = Choice    { chText    = T.pack "hello"
                , chScore   = -1
                }

ch3 = Choice    { chText    = T.pack "salut"
                , chScore   = -1
                }

ch4 = Choice    { chText    = T.pack "cus"
                , chScore   = 1
                }

ch5 = Choice    { chText    = T.pack "cao"
                , chScore   = -1
                }

a1 = SingleChoice { ascChoices = [ch1, ch2, ch3, ch5] }
a2 = MultiChoice { amcChoices = [ch1, ch2, ch3, ch4] }
a3 = TextualAnswer { ataCorrect = [T.pack "Prelude", T.pack "prelude"], ataScore = 3 }
a4 = NumericAnswer { anaCorrect = [20], anaScore = 2 }

q1 = Question { quText = T.pack "Most basic package?", quAnswer = a3 }
q2 = Question { quText = T.pack "What do you say in czech when you see somebody?", quAnswer = a2 }
q3 = Question { quText = T.pack "Hello in czech?", quAnswer = a1 }
q4 = Question { quText = T.pack "10+10?", quAnswer = a4 }

test1 = TestSet { tsName = T.pack "test", tsIntro = Just (T.pack "intro"), tsItems = [q2, q1, q4, q3] }
test2 = TestSet { tsName = T.pack "test", tsIntro = Nothing, tsItems = [q1, q2, q3, q4] }
