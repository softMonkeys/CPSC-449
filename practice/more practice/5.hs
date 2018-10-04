data TriviaQuestion = MCQ String String String String String Char | NumericQ String Float
instance Show TriviaQuestion where
  show (MCQ a b c d e f) = 2.0
   
