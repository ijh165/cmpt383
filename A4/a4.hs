import Test.QuickCheck

data Token = Num Double | Opt Operator | Err String

instance Show Token where
    show (Num n)   = "<" ++ show n ++ ">"
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
strToToken "inv"   = Opt $ Unary "inv" (1 /)
-- Converter for Binary operators
strToToken "+"     = Opt $ Binary "+" (+)
strToToken "-"     = Opt $ Binary "-" (-)
strToToken "*"     = Opt $ Binary "*" (*)
strToToken "/"     = Opt $ Binary "/" (/)
-- Converter for All operators
strToToken "+all"  = Opt $ All "+all" sum
strToToken "*all"  = Opt $ All "*all" product
-- Converter for Stack operators
strToToken "dup"   =
    Opt $ Stack "dup" dup
    where
        dup :: [Token] -> [Token]
        dup [] = [Err "dup: empty stack"]
        dup (x:xs) = x : (x:xs)
strToToken "pop"   =
    Opt $ Stack "pop" pop
    where
        pop :: [Token] -> [Token]
        pop []     = [Err "pop: empty stack"]
        pop (_:xs) = xs
strToToken "clear" = Opt $ Stack "clear" (\stack -> [])
strToToken "swap"  = 
    Opt $ Stack "swap" swap
    where
        swap :: [Token] -> [Token]
        swap []    = [Err "swap: empty stack"]
        swap [x]   = Err "swap: not enough args" : [x]
        swap (left:(right:xs)) = right : (left : xs)
-- Converter for Numbers (also handle invalid tokens)
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
    case top stack of
        Nothing  -> Err (optId ++ ": empty stack") : stack
        Just (Err _) -> stack
        Just (Num n) -> Num (f n) : tail stack
-- Evaluate Binary operators
eval (Binary optId f) stack =
    case top stack of
        Nothing                 -> Err (optId ++ ": empty stack") : stack
        Just (Err _)            -> stack
        Just (Num rightOperand) ->
            case top stack' of
                Nothing                -> Err (optId ++ ": not enough args") : stack
                Just (Err _)           -> stack
                Just (Num leftOperand) -> Num (f leftOperand rightOperand) : tail stack'
            where stack' = tail stack
-- Evaluate All operators
eval (All optId f) stack
    | null stack = Err (optId ++ ": empty stack") : stack
    | otherwise  = [Num $ f $ map (\tok -> let Num n = tok in n) stack]
-- Evaluate Stack operators
eval (Stack _ f) stack = f stack

compute :: String -> [Token]
compute str = 
    compute' (words str) []
    where
        -- Use recursion instead of fold since we need to stop upon error
        compute' :: [String] -> [Token] -> [Token]
        compute' [] stack     = stack
        compute' (x:xs) stack = 
            case strToToken x of
                Num n -> compute' xs $ Num n : stack
                Opt o -> 
                    case top stack' of
                        -- stop processing tokens upon evaluation error
                        Just (Err _) -> stack'
                        _ -> compute' xs $ stack'
                    where stack' = eval o stack
                -- stop processing tokens upon invalid token error
                Err e -> Err e : stack

-- !!! The last element is the top (marked with "*") !!!
calcStack :: String -> String
calcStack str =
    case top stack of
        Nothing  -> ""
        Just tok ->
            unwords (map show $ tail stack) ++ " " ++ show tok ++ "*"
    where stack = compute str

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

-- ============================================================================
-- Testing
-- ============================================================================

-- Given test cases
prop1  = calc "1 2 + 3 *"                     == "9.0"
prop2  = calc "1 2 3 * +"                     == "7.0"
prop3  = calc "2 sqrt 3 sqrt +"               == "3.1462643699419726"
prop4  = calc "11 dup *"                      == "121.0"
prop5  = calc "0 5 /"                         == "0.0"
prop6  = calc "5 0 /"                         == "Infinity"
prop7  = calc "2 3 + 4 2 +all"                == "11.0"
prop8  = calc "2 3 + 4 2 *all"                == "40.0"
prop9  = calc "2 3 + 4 2 clear"               == "empty stack"
prop10 = calc "2 3 inc * pop"                 == "empty stack"
prop11 = calc "3.2 sin dup * 3.2 cos dup * +" == "1.0"
prop12 = calc "2 +"                           == "+: not enough args"
prop13 = calc "dec"                           == "dec: empty stack"

-- Empty stack edge case
prop14 = calc "inc"                           == "inc: empty stack"
prop15 = calc "dec"                           == "dec: empty stack"
prop16 = calc "sqrt"                          == "sqrt: empty stack"
prop17 = calc "sin"                           == "sin: empty stack"
prop18 = calc "cos"                           == "cos: empty stack"
prop19 = calc "inv"                           == "inv: empty stack"
prop20 = calc "+"                             == "+: empty stack"
prop21 = calc "*"                             == "*: empty stack"
prop22 = calc "-"                             == "-: empty stack"
prop23 = calc "/"                             == "/: empty stack"
prop24 = calc "+all"                          == "+all: empty stack"
prop25 = calc "*all"                          == "*all: empty stack"
prop26 = calc "dup"                           == "dup: empty stack"
prop27 = calc "pop"                           == "pop: empty stack"
prop28 = calc "clear"                         == "empty stack"
prop29 = calc "swap"                          == "swap: empty stack"

-- Randomized test cases

prop30 :: NonNegative Int -> Bool
prop30 (NonNegative n) = calc (show n ++ " +")              == "+: not enough args"

prop31 :: NonNegative Int -> Bool
prop31 (NonNegative n) = calc (show n ++ " *")              == "*: not enough args"

prop32 :: NonNegative Int -> Bool
prop32 (NonNegative n) = calc (show n ++ " -")              == "-: not enough args"

prop33 :: NonNegative Int -> Bool
prop33 (NonNegative n) = calc (show n ++ " /")              == "/: not enough args"

prop34 :: NonNegative Int -> Bool
prop34 (NonNegative n) = calc (show n ++ " +all")           == calc (show n)

prop35 :: NonNegative Int -> Bool
prop35 (NonNegative n) = calc (show n ++ " *all")           == calc (show n)

prop36 :: NonNegative Int -> Bool
prop36 (NonNegative n) = calc (show n ++ " inc dec") == calc (show n)

prop37 :: NonNegative Int -> Bool
prop37 (NonNegative n) = calc (show n ++ " dec inc") == calc (show n)

prop38 :: NonNegative Int -> Bool
prop38 (NonNegative n) = 
    round (read $ calc (show n ++ " sqrt dup *") :: Double) == n

prop39 :: NonNegative Int -> Bool
prop39 (NonNegative n) =
    -- sin(-n) = -sin(n)
    calc ("0 " ++ show n ++ " - sin") == calc ("0 " ++ show n ++ " sin -")

prop40 :: NonNegative Int -> Bool
prop40 (NonNegative n) =
    -- cos(-n) = cos(n)
    calc ("0 " ++ show n ++ " - cos") == calc (show n ++ " cos")

prop41 :: NonNegative Int -> NonNegative Int -> Bool
prop41 (NonNegative x) (NonNegative y) =
    -- sin(x + y) = sin(x)cos(y) + cos(x)sin(y)
    round (read $ calc lhs :: Double) == round (read $ calc rhs :: Double)
    where
        lhs = show x ++ " " ++ show y ++ " + sin"
        rhs = show x ++ " sin " ++ show y ++ " cos * " ++ show x ++ " cos " ++ show y ++ " sin * +"

prop42 :: NonNegative Int -> Bool
prop42 (NonNegative n) =
    -- 1/(1/x) = x
    round (read $ calc (show n ++ " inv inv") :: Double) == round (read $ calc (show n) :: Double)

prop43 :: NonNegative Int -> Bool
prop43 (NonNegative n) = calc (show n ++ " dup pop") == calc (show n)

prop44 :: NonEmptyList Int -> NonEmptyList Int -> Bool
prop44 (NonEmpty lst1) (NonEmpty lst2) = 
    calc (lst1_str ++ " clear " ++ lst2_str ++ " *all") == calc (lst2_str ++ " *all")
    where
        lst1_str = unwords $ map (show . abs) lst1
        lst2_str = unwords $ map (show . abs) lst2

prop45 :: NonNegative Int -> NonNegative Int -> Bool
prop45 (NonNegative x) (NonNegative y) =
    calc (operands ++ " swap swap -") == calc (operands ++ " -") &&
    calc (operands ++ " swap swap /") == calc (operands ++ " /")
    where operands = show x ++ " " ++ show y

prop46 :: NonEmptyList Int -> NonNegative Int -> NonNegative Int -> Bool
prop46 (NonEmpty lst) (NonNegative x) (NonNegative y) =
    calc (lst_str ++ " " ++ operands ++ " +") == "invalid input" &&
    calc (lst_str ++ " " ++ operands ++ " *") == "invalid input" &&
    calc (lst_str ++ " " ++ operands ++ " -") == "invalid input" &&
    calc (lst_str ++ " " ++ operands ++ " /") == "invalid input"
    where
        lst_str = unwords $ map (show . abs) lst
        operands = show x ++ " " ++ show y

runTests = do
    putStrLn "\n===== Testing with given test cases ====="
    quickCheck prop1
    quickCheck prop2
    quickCheck prop3
    quickCheck prop4
    quickCheck prop5
    quickCheck prop6
    quickCheck prop7
    quickCheck prop8
    quickCheck prop9
    quickCheck prop10
    quickCheck prop11
    quickCheck prop12
    quickCheck prop13
    putStrLn "\n===== Testing with empty stack edge case ====="
    quickCheck prop14
    quickCheck prop15
    quickCheck prop16
    quickCheck prop17
    quickCheck prop18
    quickCheck prop19
    quickCheck prop20
    quickCheck prop21
    quickCheck prop22
    quickCheck prop23
    quickCheck prop24
    quickCheck prop25
    quickCheck prop26
    quickCheck prop27
    quickCheck prop28
    quickCheck prop29
    putStrLn "\n===== Testing with randomized test cases ====="
    quickCheck prop30
    quickCheck prop31
    quickCheck prop32
    quickCheck prop33
    quickCheck prop34
    quickCheck prop35
    quickCheck prop36
    quickCheck prop37
    quickCheck prop38
    quickCheck prop39
    quickCheck prop40
    quickCheck prop41
    quickCheck prop42
    quickCheck prop43
    quickCheck prop44
    quickCheck prop45
    quickCheck prop46
