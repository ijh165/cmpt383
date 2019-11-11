data Token = Num Double | Inc | Dec | Sqrt | Err String
    deriving (Read, Eq)

instance Show Token where
    show (Num n)   = "<" ++ (show n) ++ ">"
    show (Err str) = "<error: \"" ++ str ++ "\">"
    show Inc       = "<inc>"

isDigit :: Char -> Bool
isDigit x = '0' <= x && x <= '9'

isDouble :: String -> Bool
isDouble str = 
    (isDigit . head) str && (isDigit . last) str && 
    abs (length str' - length str) <= 1 && all isDigit str'
    where str' = filter (/= '.') str

strToToken :: String -> Token
strToToken str = 
    case str of
        "inc"  -> Inc
        "dec"  -> Dec
        "sqrt" -> Sqrt
        _      -> 
            if isDouble str 
                then Num (read str :: Double) 
                else Err "invalid token"

tokenToStr :: Token -> String
tokenToStr tok
    | tok == Inc  = "inc"
    | tok == Dec  = "dec"
    | tok == Sqrt = "sqrt"

push x stack = x : stack

pop []     = (Nothing, [])
pop (x:xs) = (Just x, xs)

top :: [a] -> Maybe a
top []     = Nothing
top (x:xs) = Just x

evalUnary :: (Double -> Double) -> [Token] -> [Token]
evalUnary f stack =
    let operand = top stack in
    case operand of
        Nothing -> Err "not enough args" : stack
        (Just tok) ->
            case tok of
                (Err _) -> stack
                (Num n) -> 
                    Num (f n) : stack'
                    where stack'  = tail stack

-- evalBinary :: (Double -> Double -> Double) -> [Token] -> [Token]
-- evalBinary f stack =
--     case
--     where
--         rightOperand = top stack
--         stack' = tail stack
--         leftOperand

process stack word = 
    case strToToken word of
        (Num n) -> Num n : stack
        Inc     -> evalUnary (+ 1) stack

compute :: String -> [Token]
compute str = foldl process [] $ words str

-- calcStack :: String -> String
-- calcStack str = foldl ((++ " ") . tokenToStr) "" $ map tokenToStr $ compute str


calc :: String -> String
calc str = ""
