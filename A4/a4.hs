import Test.QuickCheck

data Token = Num Double | Opt Operator | Err String

instance Show Token where
    show (Num n)   = "<" ++ (show n) ++ ">"
    show (Opt o)   = "<" ++ show o ++ ">"
    show (Err e)   = "<error: " ++ e ++ ">"

data Operator = 
    Unary String (Double -> Double)            |
    Binary String (Double -> Double -> Double) |
    All String ([Double] -> Double)            |
    Stack String ([Token] -> [Token])

instance Show Operator where
    show (Unary optId _)  = show optId
    show (Binary optId _) = show optId
    show (All optId _)    = show optId
    show (Stack optId _)  = show optId

strToToken :: String -> Token
-- Converter for Unary operators
strToToken "inc"   = Opt $ Unary "inc" (+ 1)
strToToken "dec"   = Opt $ Unary "dec" (+ (-1))
strToToken "sqrt"  = Opt $ Unary "sqrt" sqrt
strToToken "sin"   = Opt $ Unary "sin" sin
strToToken "cos"   = Opt $ Unary "cos" cos
strToToken "tan"   = Opt $ Unary "tan" tan
-- Converter for Binary operators
strToToken "+"     = Opt $ Binary "+" (+)
strToToken "-"     = Opt $ Binary "-" (-)
strToToken "*"     = Opt $ Binary "-" (*)
strToToken "/"     = Opt $ Binary "-" (/)
-- Converter for All operators
strToToken "+all"  = Opt $ All "+all" sum
strToToken "*all"  = Opt $ All "*all" product
-- Converter for Stack operators
strToToken "dup"   =
    Opt $ Stack "dup" dup
    where
        dup stack
            | null stack = Err "dup: empty stack" : stack
            | otherwise = head stack : stack
strToToken "pop"   =
    Opt $ Stack "pop" pop
    where
        pop stack
            | null stack = Err "pop: empty stack" : stack
            | otherwise  = tail stack
strToToken "clear" = Opt $ Stack "clear" (\stack -> [])
strToToken "swap"  = 
    Opt $ Stack "swap" swap
    where
        swap :: [Token] -> [Token]
        swap stack = 
            if length operands == numArgs
                then reverse operands ++ drop numArgs stack
                else Err "swap: not enough args" : stack
            where
                numArgs = 2
                operands = take numArgs stack
strToToken str 
    | isDouble str = Num (read str :: Double) 
    | otherwise    = Err $ "invalid token: " ++ str
    -- use where to make helper functions only accessible here
    where
        -- helper for isDouble to check if character is digit.
        isDigit :: Char -> Bool
        isDigit x = '0' <= x && x <= '9'
        -- helper to check if string convertible to double.
        isDouble :: String -> Bool
        isDouble str = 
            (isDigit . head) str && (isDigit . last) str && 
            abs (length str' - length str) <= 1 && all isDigit str'
            where str' = filter (/= '.') str

-- Return stack top or Nothing if empty
top :: [a] -> Maybe a
top []     = Nothing
top (x:xs) = Just x

eval :: Operator -> [Token] -> [Token]
-- Evaluate Unary operators
eval (Unary optId f) stack =
    let operand = top stack in
    case operand of
        Nothing  -> Err (optId ++ ": not enough args") : stack
        Just tok ->
            case tok of
                Err _ -> stack
                Num n -> 
                    Num (f n) : stack'
                    where stack'  = tail stack
-- Evaluate Binary operators
eval (Binary optId f) stack =
    let numArgs = 2
        operands = take numArgs stack
        in if length operands < numArgs
            then Err (optId ++ ": not enough args") : stack
            else
                let rightOperand = head operands
                    leftOperand = (head . tail) operands
                in case rightOperand of
                    Err _        -> stack
                    Num rightNum ->
                        let (Num leftNum) = leftOperand
                        in Num (f leftNum rightNum) : drop numArgs stack
-- Evaluate All operators
eval (All optId f) stack
    | null stack = Err (optId ++ ": not enough args") : stack
    | otherwise  = [Num $ f $ map (\tok -> let Num n = tok in n) stack]
-- Evaluate Stack operators
eval (Stack optId f) stack = f stack

compute :: String -> [Token]
compute str = 
    compute' (words str) []
    where
        -- Use recursion instead of fold since we need to stop upon error
        compute' [] stack     = stack
        compute' (x:xs) stack = 
            case strToToken x of
                Num n -> compute' xs $ Num n : stack
                Opt o -> 
                    case top stack' of
                        Just (Err _) -> stack'
                        _ -> compute' xs $ stack'
                    where stack' = eval o stack
                -- stop processing tokens upon invalid token error
                Err e -> Err e : stack

-- First element is top.
calcStack :: String -> String
calcStack str = foldl (\accum next -> accum ++ show next ++ "; ") "" $ compute str

calc :: String -> String
calc str =
    case top stack of
        Nothing      -> "empty stack"
        Just (Err e) -> e
        Just (Num n) ->
            if length stack == 1
                then show n
                else "invalid input"
    where stack = compute str

prop1 = calc "1 2 + 3 *"       == "9.0"
prop2 = calc "1 2 3 * +"       == "7.0"
prop3 = calc "2 sqrt 3 sqrt +" == "3.1462643699419726"

runTests = do
    quickCheck prop1
    quickCheck prop2
    quickCheck prop3
