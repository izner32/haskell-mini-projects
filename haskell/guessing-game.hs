-- here, we learn about IO 
-- here, we learn about declaring variables
-- here, we learn about looping with recursion 

-- run game function 
runGame :: Int -> IO () -- function_name :: argument1_type -> return_type 
runGame incorrectGuesses = do -- function_name parameter  = function_definition 
    -- variable definition 
    let secretNumber = 5 

    -- conditional statement: must have if, then, else 
    if incorrectGuesses >= 3 
    then putStrLn "You run out of guesses :("
    else do 
        guessSecretNumber <- getLine 

        if guessSecretNumber == secretNumber 
        then putStrLn "Yay, you win"
        else runGame (incorrectGuesses + 1) -- using recursion because there's no other way to loop in haskell 

-- main thread function 
main :: IO () -- function signature: format of function 
-- function definition 
main = do -- do keyword is typically used instead of curly brackets to specify scope 
    runGame -- just like in python you indent to specify the scope it belongs to 