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
prop_given_1  = calc "1 2 + 3 *"                     == "9.0"
prop_given_2  = calc "1 2 3 * +"                     == "7.0"
prop_given_3  = calc "2 sqrt 3 sqrt +"               == "3.1462643699419726"
prop_given_4  = calc "11 dup *"                      == "121.0"
prop_given_5  = calc "0 5 /"                         == "0.0"
prop_given_6  = calc "5 0 /"                         == "Infinity"
prop_given_7  = calc "2 3 + 4 2 +all"                == "11.0"
prop_given_8  = calc "2 3 + 4 2 *all"                == "40.0"
prop_given_9  = calc "2 3 + 4 2 clear"               == "empty stack"
prop_given_10 = calc "2 3 inc * pop"                 == "empty stack"
prop_given_11 = calc "3.2 sin dup * 3.2 cos dup * +" == "1.0"
prop_given_12 = calc "2 +"                           == "+: not enough args"
prop_given_13 = calc "dec"                           == "dec: empty stack"

-- Empty stack edge case
prop_empty_1 = calc ""                              == "empty stack"
prop_empty_2 = calc "inc"                           == "inc: empty stack"
prop_empty_3 = calc "dec"                           == "dec: empty stack"
prop_empty_4 = calc "sqrt"                          == "sqrt: empty stack"
prop_empty_5 = calc "sin"                           == "sin: empty stack"
prop_empty_6 = calc "cos"                           == "cos: empty stack"
prop_empty_7 = calc "inv"                           == "inv: empty stack"
prop_empty_8 = calc "+"                             == "+: empty stack"
prop_empty_9 = calc "*"                             == "*: empty stack"
prop_empty_10 = calc "-"                             == "-: empty stack"
prop_empty_11 = calc "/"                             == "/: empty stack"
prop_empty_12 = calc "+all"                          == "+all: empty stack"
prop_empty_13 = calc "*all"                          == "*all: empty stack"
prop_empty_14 = calc "dup"                           == "dup: empty stack"
prop_empty_15 = calc "pop"                           == "pop: empty stack"
prop_empty_16 = calc "clear"                         == "empty stack"
prop_empty_17 = calc "swap"                          == "swap: empty stack"

-- Randomized test cases

prop_rng_1 :: NonNegative Int -> Bool
prop_rng_1 (NonNegative n) = calc (show n ++ " +")              == "+: not enough args"

prop_rng_2 :: NonNegative Int -> Bool
prop_rng_2 (NonNegative n) = calc (show n ++ " *")              == "*: not enough args"

prop_rng_3 :: NonNegative Int -> Bool
prop_rng_3 (NonNegative n) = calc (show n ++ " -")              == "-: not enough args"

prop_rng_4 :: NonNegative Int -> Bool
prop_rng_4 (NonNegative n) = calc (show n ++ " /")              == "/: not enough args"

prop_rng_5 :: NonNegative Int -> Bool
prop_rng_5 (NonNegative n) = calc (show n ++ " +all")           == calc (show n)

prop_rng_6 :: NonNegative Int -> Bool
prop_rng_6 (NonNegative n) = calc (show n ++ " *all")           == calc (show n)

prop_rng_7 :: NonNegative Int -> Bool
prop_rng_7 (NonNegative n) = calc (show n ++ " inc dec") == calc (show n)

prop_rng_8 :: NonNegative Int -> Bool
prop_rng_8 (NonNegative n) = calc (show n ++ " dec inc") == calc (show n)

prop_rng_9 :: NonNegative Int -> Bool
prop_rng_9 (NonNegative n) = 
    round (read $ calc (show n ++ " sqrt dup *") :: Double) == n

prop_rng_10 :: NonNegative Int -> Bool
prop_rng_10 (NonNegative n) =
    -- sin(-n) = -sin(n)
    calc ("0 " ++ show n ++ " - sin") == calc ("0 " ++ show n ++ " sin -")

prop_rng_11 :: NonNegative Int -> Bool
prop_rng_11 (NonNegative n) =
    -- cos(-n) = cos(n)
    calc ("0 " ++ show n ++ " - cos") == calc (show n ++ " cos")

prop_rng_12 :: NonNegative Int -> NonNegative Int -> Bool
prop_rng_12 (NonNegative x) (NonNegative y) =
    -- sin(x + y) = sin(x)cos(y) + cos(x)sin(y)
    round (read $ calc lhs :: Double) == round (read $ calc rhs :: Double)
    where
        lhs = show x ++ " " ++ show y ++ " + sin"
        rhs = show x ++ " sin " ++ show y ++ " cos * " ++ show x ++ " cos " ++ show y ++ " sin * +"

prop_rng_13 :: NonNegative Int -> Bool
prop_rng_13 (NonNegative n) =
    -- 1/(1/x) = x
    round (read $ calc (show n ++ " inv inv") :: Double) == round (read $ calc (show n) :: Double)

prop_rng_14 :: NonNegative Int -> Bool
prop_rng_14 (NonNegative n) = calc (show n ++ " dup pop") == calc (show n)

prop_rng_15 :: NonEmptyList Int -> NonEmptyList Int -> Bool
prop_rng_15 (NonEmpty lst1) (NonEmpty lst2) = 
    calc (lst1_str ++ " clear " ++ lst2_str ++ " *all") == calc (lst2_str ++ " *all")
    where
        lst1_str = unwords $ map (show . abs) lst1
        lst2_str = unwords $ map (show . abs) lst2

prop_rng_16 :: NonNegative Int -> NonNegative Int -> Bool
prop_rng_16 (NonNegative x) (NonNegative y) =
    calc (operands ++ " swap swap -") == calc (operands ++ " -") &&
    calc (operands ++ " swap swap /") == calc (operands ++ " /")
    where operands = show x ++ " " ++ show y

prop_rng_17 :: NonEmptyList Int -> NonNegative Int -> NonNegative Int -> Bool
prop_rng_17 (NonEmpty lst) (NonNegative x) (NonNegative y) =
    calc (lst_str ++ " " ++ operands ++ " +") == "invalid input" &&
    calc (lst_str ++ " " ++ operands ++ " *") == "invalid input" &&
    calc (lst_str ++ " " ++ operands ++ " -") == "invalid input" &&
    calc (lst_str ++ " " ++ operands ++ " /") == "invalid input"
    where
        lst_str = unwords $ map (show . abs) lst
        operands = show x ++ " " ++ show y

runTests = do
    putStrLn "\n===== Testing with given test cases ====="
    quickCheck prop_given_1
    quickCheck prop_given_2
    quickCheck prop_given_3
    quickCheck prop_given_4
    quickCheck prop_given_5
    quickCheck prop_given_6
    quickCheck prop_given_7
    quickCheck prop_given_8
    quickCheck prop_given_9
    quickCheck prop_given_10
    quickCheck prop_given_11
    quickCheck prop_given_12
    quickCheck prop_given_13
    putStrLn "\n===== Testing with empty stack edge case ====="
    quickCheck prop_empty_1
    quickCheck prop_empty_2
    quickCheck prop_empty_3
    quickCheck prop_empty_4
    quickCheck prop_empty_5
    quickCheck prop_empty_6
    quickCheck prop_empty_7
    quickCheck prop_empty_8
    quickCheck prop_empty_9
    quickCheck prop_empty_10
    quickCheck prop_empty_11
    quickCheck prop_empty_12
    quickCheck prop_empty_13
    quickCheck prop_empty_14
    quickCheck prop_empty_15
    quickCheck prop_empty_16
    quickCheck prop_empty_17
    putStrLn "\n===== Testing with randomized test cases ====="
    quickCheck prop_rng_1
    quickCheck prop_rng_2
    quickCheck prop_rng_3
    quickCheck prop_rng_4
    quickCheck prop_rng_5
    quickCheck prop_rng_6
    quickCheck prop_rng_7
    quickCheck prop_rng_8
    quickCheck prop_rng_9
    quickCheck prop_rng_10
    quickCheck prop_rng_11
    quickCheck prop_rng_12
    quickCheck prop_rng_13
    quickCheck prop_rng_14
    quickCheck prop_rng_15
    quickCheck prop_rng_16
    quickCheck prop_rng_17
