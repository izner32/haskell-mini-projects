-- here, we learn about conditional statements with ifs, guards, case, and pattern matching

-- using regular if statement 
-- remember there is no else if in haskell, so it's kinda like how you treat ternary operator in other languages
runCalculatorUsingIf :: Int -> Int -> String -> Int 
runCalculatorUsingIf num1 num2 operation = do 
    if operation == "add"
    then num1 + num2 
    else do 
        if operation == "subtract"
        then num1 - num2 
        else do 
            if operation == "multiply"
            then num1 * num2 
            else do 
                if operation == "divide"
                then num1 `div` num2 -- interestingly, operator "/" doesn't allow two integers as its argument, so we instead use `div` for dividing integers
                else 1

-- using guard, basically just a syntactical sugar for case 
runCalculatorUsingGuard :: Int -> Int -> String -> Int 
runCalculatorUsingGuard num1 num2 operation 
    | operation == "add" = num1 + num2 
    | operation == "subtract" = num1 - num2 
    | operation == "multiply" = num1 * num2 
    | operation == "divide" = num1 `div` num2 
    | otherwise = 1 

-- using case
runCalculatorUsingCase :: Int -> Int -> String -> Int 
runCalculatorUsingCase num1 num2 operation = do
    case operation of "add" -> num1 + num2  
                      "subtract" -> num1 - num2  
                      "multiply" -> num1 * num2 
                      "divide" -> num1 `div` num2 

-- using pattern matching 
runCalculatorUsingPatternMatching :: Int -> Int -> String -> Int
runCalculatorUsingPatternMatching num1 num2 "add" = num1 + num2 
runCalculatorUsingPatternMatching num1 num2 "subtract" = num1 - num2 
runCalculatorUsingPatternMatching num1 num2 "multiply" = num1 * num2 
runCalculatorUsingPatternMatching num1 num2 "divide" = num1 `div` num2 


main :: IO () 
main = do 
    putStrLn (show (runCalculatorUsingIf 10 5 "add"))