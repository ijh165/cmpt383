data Token = Num Double | Opt Operator | Err String

instance Show Token where
    show (Num n)   = "<" ++ (show n) ++ ">"
    show (Opt o)   = "<" ++ show o ++ ">"
    show (Err e)   = "<error: \"" ++ e ++ "\">"

data Operator = 
    Unary String (Double -> Double)            | 
    Binary String (Double -> Double -> Double) | 
    All String ([Double] -> Double)             |
    Stack String ([Token] -> [Token])

instance Show Operator where
    show (Unary optId _)  = show optId
    show (Binary optId _) = show optId
    show (All optId _)    = show optId
    show (Stack optId _)  = show optId

-- data UnaryOptType = Inc | Dec | Sqrt | Sin | Cos | Tan
-- data BinaryOptType = Add | Sub
-- data AllOptType = AddAll | MultAll

strToToken :: String -> Token
-- Converter for Unary operators
strToToken "inc"  = Opt $ Unary "inc" (+ 1)
strToToken "dec"  = Opt $ Unary "dec" (+ (-1))
strToToken "sqrt" = Opt $ Unary "sqrt" sqrt
strToToken "sin"  = Opt $ Unary "sin" sin
strToToken "cos"  = Opt $ Unary "cos" cos
strToToken "tan"  = Opt $ Unary "tan" tan
-- Converter for Binary operators
strToToken "+"    = Opt $ Binary "+" (+)
strToToken "-"    = Opt $ Binary "-" (-)
strToToken "*"    = Opt $ Binary "-" (*)
strToToken "/"    = Opt $ Binary "-" (/)
-- Converter for All operators
strToToken "+all" = Opt $ All "+all" sum
strToToken "*all" = Opt $ All "*all" product
-- Converter for Stack operators
strToToken str 
    | isDouble str = Num (read str :: Double) 
    | otherwise    = Err "invalid token"
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

top :: [a] -> Maybe a
top []     = Nothing
top (x:xs) = Just x

eval :: Operator -> [Token] -> [Token]
-- Evaluate Unary operators
eval (Unary optId f) stack =
    let operand = top stack in
    case operand of
        Nothing    -> Err (optId ++ ": not enough args") : stack
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
                    Err _ -> stack
                    Num rightNum ->
                        let (Num leftNum) = leftOperand
                        in Num (f leftNum rightNum) : drop numArgs stack
    -- case rightOperand of
    --     Nothing  -> notEnoughArgsErr
    --     Just tok ->
    --         case tok of
    --             Err _ -> stack
    --             Num right ->
    --                 case leftOperand of
    --                     Nothing   -> notEnoughArgsErr
    --                     Just tok' ->
    --                         case tok' of
    --                             Err _ -> stack
    --                             Num left -> Num (f left right) : tail stack'
    --                 where
    --                     stack' = tail stack
    --                     leftOperand = top stack'
    -- where
    --     notEnoughArgsErr = Err (optId ++ ": not enough args") : stack
    --     rightOperand = top stack

compute :: String -> [Token]
compute str = 
    -- Use recursion instead of fold since we need to stop upon error
    compute' (words str) []
    where
        compute' [] stack     = stack
        compute' (x:xs) stack = 
            case strToToken x of
                Num n -> compute' xs $ Num n : stack
                Opt o -> compute' xs $ eval o stack
                -- stop processing tokens when error detected
                Err e -> Err e : stack

calcStack :: String -> String
calcStack str = show $ compute str

calc :: String -> String
calc str = 
    case top $ compute str of
        Nothing -> "empty stack"
        (Just (Err e)) -> e
        (Just (Num n)) -> show n
