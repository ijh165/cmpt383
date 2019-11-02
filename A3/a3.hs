import Test.QuickCheck

-- ============================================================================
-- Basic Functions
-- ============================================================================

-- 1.
snoc :: a -> [a] -> [a]
snoc x []  = [x]
snoc x (first:rest) = first : snoc x rest

-- 2.
myappend :: [a] -> [a] -> [a]
myappend [] lst = lst
myappend (x:xs) lst = x : myappend xs lst

-- 3.
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs `myappend` [x]

-- Helper to check for prime
is_prime :: Int -> Bool
is_prime n
    | n < 2     = False
    | otherwise = all (\i -> n `mod` i /= 0) [2..n-1]

-- Helper to reverse number (i.e. 123 -> 321)
reverse_num :: Int -> Int
reverse_num n = read (myreverse (show n)) :: Int

-- Helper to check if number is emirp
is_emirp :: Int -> Bool
is_emirp n = is_prime n && is_prime n_rev && n_rev /= n
             where n_rev = reverse_num n

-- 4.
count_emirps :: Int -> Int
count_emirps n = length (filter is_emirp [13..n])

-- 5.
biggest_sum :: [[Int]] -> [Int]
biggest_sum lst = greatest (\item -> foldl (+) 0 item) lst

-- 6.
greatest :: (a -> Int) -> [a] -> a
greatest f (x:xs) = 
    foldl maximize x xs
    where 
        maximize accum next =
            if (f next) > (f accum) 
                then next 
                else accum

-- ============================================================================
-- Basic Bits
-- ============================================================================

-- 7.
is_bit :: Int -> Bool
is_bit x = x == 1 || x == 0

-- 8.
flip_bit :: Int -> Int
flip_bit x
    | is_bit x  = (x + 1) `mod` 2
    | otherwise = error $ show x ++ " is not a bit."

-- 9. a)
is_bit_seq1 :: [Int] -> Bool
is_bit_seq1 x
    | null x    = True
    | otherwise = is_bit (head x) && is_bit_seq1 (tail x)

-- 9. b)
is_bit_seq2 :: [Int] -> Bool
is_bit_seq2 x = 
    if null x then True else is_bit (head x) && is_bit_seq1 (tail x)

-- 9. c)
is_bit_seq3 :: [Int] -> Bool
is_bit_seq3 x = all is_bit x

-- 10. a)
invert_bits1 :: [Int] -> [Int]
invert_bits1 []     = []
invert_bits1 (x:xs) = flip_bit x : invert_bits1 xs

-- 10. b)
invert_bits2 :: [Int] -> [Int]
invert_bits2 x = map flip_bit x

-- 10. c)
invert_bits3 :: [Int] -> [Int]
invert_bits3 x = [flip_bit bit | bit <- x]

-- 11.
bit_count :: [Int] -> (Int, Int)
bit_count x = 
    foldl 
    (\(zeroes, ones) next ->
        case next of
            0 -> (zeroes + 1, ones)
            1 -> (zeroes, ones + 1)
            _ -> error $ show next ++ " is not a bit.")
    (0,0) x

-- Helper to convert num to bit sequence
to_bit_seq n = 
    (reverse . to_bit_seq') n
    where 
        to_bit_seq' 0  = []
        to_bit_seq' n' = 
            remainder : to_bit_seq' quotient
            where (quotient, remainder) = n' `divMod` 2 

-- Helper to add 0 padding to bit_seq so that its size is bit_size
apply_padding :: [Int] -> Int -> [Int]
apply_padding bit_seq bit_size =
    take padding_size (repeat 0) ++ bit_seq
    where padding_size = bit_size - (length bit_seq)

-- 12.
all_basic_bit_seqs :: Int -> [[Int]]
all_basic_bit_seqs n
    | n < 1     = []
    | otherwise = map (\item -> apply_padding (to_bit_seq item) n) [0..2^n-1]

-- ============================================================================
-- A Custom List Data Type
-- ============================================================================

data List a = Empty | Cons a (List a)
    deriving Show

-- 13.
toList :: [a] -> List a
toList lst = foldr Cons Empty lst

-- 14.
toHaskellList :: List a -> [a]
toHaskellList Empty             = []
toHaskellList (Cons first rest) = first : toHaskellList rest

-- 15.
append :: List a -> List a -> List a
append Empty lstB             = lstB
append (Cons first rest) lstB = Cons first (append rest lstB)

-- 16.
removeAll :: (a -> Bool) -> List a -> List a
removeAll f Empty             = Empty
removeAll f (Cons first rest) 
    | f first   = restRemoved 
    | otherwise = Cons first restRemoved
    where restRemoved = removeAll f rest

-- 17.
sort :: Ord a => List a -> List a
sort Empty             = Empty
sort (Cons first rest) =
    smalls `append` (Cons first Empty) `append` bigs
    where smalls = sort $ removeAll (\item -> item > first) rest
          bigs   = sort $ removeAll (\item -> item <= first) rest

-- ============================================================================
-- Basic Functions Test
-- ============================================================================

-- 1. snoc test

prop_snoc_1 :: Int -> [Int] -> Bool
prop_snoc_1 x lst = head (reverse (snoc x lst)) == x

prop_snoc_2 :: Int -> [Int] -> Bool
prop_snoc_2 x lst = reverse (snoc x lst) == x : (reverse lst)

snoc_test = do
    quickCheck prop_snoc_1
    quickCheck prop_snoc_2

-- 2. myappend test

prop_myappend_1 :: [Int] -> [Int] -> Bool
prop_myappend_1 lst1 lst2 = myappend lst1 lst2 == lst1 ++ lst2

prop_myappend_2 :: [Int] -> [Int] -> Bool
prop_myappend_2 lst1 lst2 = 
    reverse (lst1 `myappend` lst2) == reverse lst2 `myappend` reverse lst1

prop_myappend_3 :: [Int] -> [Int] -> Bool
prop_myappend_3 lst1 lst2 = 
    length (lst1 `myappend` lst2) == length lst1 + length lst2

myappend_test = do
    quickCheck prop_myappend_1
    quickCheck prop_myappend_2
    quickCheck prop_myappend_3

-- 3. myreverse test

prop_myreverse_1 :: [Int] -> Bool
prop_myreverse_1 lst = myreverse (myreverse lst) == lst

prop_myreverse_2 :: [Int] -> Bool
prop_myreverse_2 lst = length (myreverse lst) == length lst

prop_myreverse_3 :: [Int] -> [Int] -> Bool
prop_myreverse_3 lst1 lst2 = 
    myreverse (lst1 ++ lst2) == myreverse lst2 ++ myreverse lst1

myreverse_test = do
    quickCheck prop_myreverse_1
    quickCheck prop_myreverse_2
    quickCheck prop_myreverse_3

-- 4. count_emirps test

prop_count_emirps_1 :: Int -> Bool
prop_count_emirps_1 n
    -- first 8 emirps
    | n < 13    = emirps == 0
    | n < 17    = emirps == 1
    | n < 31    = emirps == 2
    | n < 37    = emirps == 3
    | n < 71    = emirps == 4
    | n < 73    = emirps == 5
    | n < 79    = emirps == 6
    | n < 97    = emirps == 7
    | n < 107   = emirps == 8
    -- before 36th emirp
    | n < 991   = 8 < emirps && emirps < 36 
    -- 36th emirp
    | n < 1009  = emirps == 36
    -- after 36th emirp
    | otherwise = emirps > 36
    -- local var so only count_emirps once
    where emirps = count_emirps n

count_emirps_test = do
    quickCheckWith stdArgs { maxSize = cap, maxSuccess = cap } prop_count_emirps_1
    where cap = 1500

-- 5. biggest_sum test
prop_biggest_sum_1 :: NonEmptyList Int -> Bool
prop_biggest_sum_1 (NonEmpty lst) = 
    biggest_sum lst_of_lsts == greatest sum lst_of_lsts
    where lst_of_lsts = [[head lst], lst, tail lst, [last lst]]

biggest_sum_test = do
    quickCheck prop_biggest_sum_1

-- 6. greatest test

prop_greatest_1 :: (Positive Int) -> Bool
prop_greatest_1 (Positive n) = greatest id [1..n] == n

prop_greatest_2 :: NonEmptyList Int -> Bool
prop_greatest_2 (NonEmpty lst) = greatest id lst == greatest id (reverse lst)

prop_greatest_3 :: NonEmptyList Int -> Bool
prop_greatest_3 (NonEmpty lst) = 
    greatest sum [lst, lst_rev] == lst &&
    greatest sum [lst_rev, lst] == lst_rev
    where lst_rev = reverse lst

prop_greatest_4 :: NonEmptyList Int -> Bool
prop_greatest_4 (NonEmpty lst) = 
    greatest sum lst_of_lsts == abs_lst && greatest length lst_of_lsts == abs_lst
    where 
        abs_lst = map abs lst
        lst_of_lsts = [abs_lst, filter even abs_lst, filter odd abs_lst]

greatest_test = do
    quickCheck prop_greatest_1
    quickCheck prop_greatest_2
    quickCheck prop_greatest_3
    quickCheck prop_greatest_4

-- ============================================================================
-- Basic Bits Test
-- ============================================================================

-- 7. is_bit test

prop_is_bit_1 :: Int -> Property
prop_is_bit_1 x = x == 0 || x == 1 ==> is_bit x == True

prop_is_bit_2 :: Int -> Property
prop_is_bit_2 x = x /= 0 && x /= 1 ==> is_bit x == False

is_bit_test = do
    quickCheckWith stdArgs { maxSize = cap } prop_is_bit_1
    quickCheck prop_is_bit_2
    where cap = 2

-- 8. flip_bit test

prop_flip_bit_1 :: Positive Int -> Property
prop_flip_bit_1 (Positive x) = is_bit x ==> flip_bit (flip_bit x) == x 

prop_flip_bit_2 :: Positive Int -> Property
prop_flip_bit_2 (Positive x) = is_bit x ==> flip_bit x == (x + 1) `mod` 2

prop_flip_bit_3_fail :: Int -> Property
prop_flip_bit_3_fail x = (not . is_bit) x ==> flip_bit (flip_bit x) == x

flip_bit_test = do
    quickCheckWith stdArgs { maxSize = cap } prop_flip_bit_1
    quickCheckWith stdArgs { maxSize = cap } prop_flip_bit_2
    quickCheck $ expectFailure prop_flip_bit_3_fail
    where cap = 2

-- 9. is_bit_seq test

prop_is_bit_seq_1 :: [Int] -> Bool
prop_is_bit_seq_1 lst = 
    is_bit_seq1 lst == is_bit_seq2 lst && 
    is_bit_seq2 lst == is_bit_seq3 lst

prop_is_bit_seq_2 :: ([Int] -> Bool) -> [Int] -> Bool
prop_is_bit_seq_2 is_bit_seq_fn lst = 
    is_bit_seq_fn lst == is_bit_seq_fn (reverse lst)

prop_is_bit_seq_3 :: ([Int] -> Bool) -> (Positive Int) -> Bool
prop_is_bit_seq_3 is_bit_seq_fn (Positive n) = 
    is_bit_seq_fn bit_seq == is_bit_seq_fn (reverse bit_seq)
    where bit_seq = to_bit_seq n

prop_is_bit_seq_4 :: ([Int] -> Bool) -> [Int] -> Bool
prop_is_bit_seq_4 is_bit_seq_fn lst
    | null no_bits_lst = is_bit_seq_fn no_bits_lst
    | otherwise        = not $ is_bit_seq_fn no_bits_lst
    where no_bits_lst = filter (not . is_bit) lst

is_bit_seq_test = do
    quickCheck prop_is_bit_seq_1
    quickCheck $ prop_is_bit_seq_2 is_bit_seq1
    quickCheck $ prop_is_bit_seq_2 is_bit_seq2
    quickCheck $ prop_is_bit_seq_2 is_bit_seq3
    quickCheck $ prop_is_bit_seq_3 is_bit_seq1
    quickCheck $ prop_is_bit_seq_3 is_bit_seq2
    quickCheck $ prop_is_bit_seq_3 is_bit_seq3
    quickCheck $ prop_is_bit_seq_4 is_bit_seq1
    quickCheck $ prop_is_bit_seq_4 is_bit_seq2
    quickCheck $ prop_is_bit_seq_4 is_bit_seq3

-- 10. invert_bits test

prop_invert_bits_1 :: (Positive Int) -> Bool
prop_invert_bits_1 (Positive n) = 
    invert_bits1 bit_seq == invert_bits2 bit_seq &&
    invert_bits2 bit_seq == invert_bits3 bit_seq
    where bit_seq = to_bit_seq n

prop_invert_bits_2 :: ([Int] -> [Int]) -> (Positive Int) -> Bool
prop_invert_bits_2 invert_bits_fn (Positive n) = 
    invert_bits_fn (invert_bits_fn bit_seq) == bit_seq
    where bit_seq = to_bit_seq n

prop_invert_bits_3 :: ([Int] -> [Int]) -> (Positive Int) -> Bool
prop_invert_bits_3 invert_bits_fn (Positive n) =
    length (invert_bits_fn bit_seq) == length bit_seq
    where bit_seq = to_bit_seq n

prop_invert_bits_4_fail :: ([Int] -> [Int]) -> [Int] -> Bool
prop_invert_bits_4_fail invert_bits_fn lst =
    invert_bits_fn (invert_bits_fn no_bits_lst) == no_bits_lst
    where no_bits_lst = filter (not . is_bit) lst

invert_bits_test = do
    quickCheck prop_invert_bits_1
    quickCheck $ prop_invert_bits_2 invert_bits1
    quickCheck $ prop_invert_bits_2 invert_bits2
    quickCheck $ prop_invert_bits_2 invert_bits3
    quickCheck $ prop_invert_bits_3 invert_bits1
    quickCheck $ prop_invert_bits_3 invert_bits2
    quickCheck $ prop_invert_bits_3 invert_bits3
    quickCheck $ expectFailure $ prop_invert_bits_4_fail invert_bits1
    quickCheck $ expectFailure $ prop_invert_bits_4_fail invert_bits2
    quickCheck $ expectFailure $ prop_invert_bits_4_fail invert_bits3

-- 11. bit_count test

prop_bit_count_1 :: (Positive Int) -> Bool
prop_bit_count_1 (Positive n) = 
    bit_count bit_seq == bit_count (reverse bit_seq)
    where bit_seq = to_bit_seq n

prop_bit_count_2 :: (Positive Int) -> Bool
prop_bit_count_2 (Positive n) = 
    zeroes == quotient && ones == quotient + remainder
    where 
        bit_seq = map (\item -> item `mod` 2) [1..n]
        (zeroes, ones) = bit_count bit_seq
        (quotient, remainder) = (n `divMod` 2)

prop_bit_count_3_fail :: [Int] -> Bool
prop_bit_count_3_fail lst = 
    bit_count no_bits_lst == (0, 0) -- valid case when no_bits_lst empty
    where no_bits_lst = filter (not . is_bit) lst

bit_count_test = do
    quickCheck prop_bit_count_1
    quickCheck prop_bit_count_2
    quickCheck $ expectFailure prop_bit_count_3_fail

-- 12. all_basic_bit_seqs test

prop_all_basic_bit_seqs_1 :: Int -> Property
prop_all_basic_bit_seqs_1 n = n < 1 ==> all_basic_bit_seqs n == []

prop_all_basic_bit_seqs_2 :: (Positive Int) -> Bool
prop_all_basic_bit_seqs_2 (Positive n) = length (all_basic_bit_seqs n) == 2^n

prop_all_basic_bit_seqs_3 :: ([Int] -> Bool) -> Int -> Bool
prop_all_basic_bit_seqs_3 is_bit_seq_fn n = 
    all is_bit_seq_fn (all_basic_bit_seqs n)

prop_all_basic_bit_seqs_4 :: ([Int] -> [Int]) -> (Positive Int) -> Bool
prop_all_basic_bit_seqs_4 invert_bits_fn (Positive n) = 
    length lower_half == length upper_half &&
    all (\(lower, upper) -> invert_bits_fn lower == upper) zipped
    where
        (lower_half, upper_half) = splitAt (2^(n-1)) $ all_basic_bit_seqs n
        make_tuple a b = (a, b)
        zipped = zipWith make_tuple lower_half (reverse upper_half)

all_basic_bit_seqs_test = do
    quickCheck prop_all_basic_bit_seqs_1
    quickCheckWith stdArgs { maxSize = cap } prop_all_basic_bit_seqs_2
    quickCheckWith stdArgs { maxSize = cap } $ prop_all_basic_bit_seqs_3 is_bit_seq1
    quickCheckWith stdArgs { maxSize = cap } $ prop_all_basic_bit_seqs_3 is_bit_seq2
    quickCheckWith stdArgs { maxSize = cap } $ prop_all_basic_bit_seqs_3 is_bit_seq3
    quickCheckWith stdArgs { maxSize = cap } $ prop_all_basic_bit_seqs_4 invert_bits1
    quickCheckWith stdArgs { maxSize = cap } $ prop_all_basic_bit_seqs_4 invert_bits2
    quickCheckWith stdArgs { maxSize = cap } $ prop_all_basic_bit_seqs_4 invert_bits3
    where cap = 16 -- limit n to 16 due to O(2^n) time complexity

-- ============================================================================
-- Custom List Test
-- ============================================================================

-- 13. toList test

prop_toList_1 :: NonEmptyList Int -> Bool
prop_toList_1 (NonEmpty lst) = first == head lst where (Cons first rest) = toList lst

prop_toList_2 :: NonEmptyList Int -> Bool
prop_toList_2 (NonEmpty lst) = first == last lst
    where (Cons first rest) = toList (reverse lst)

toList_test = do
    quickCheck prop_toList_1
    quickCheck prop_toList_2

-- 14. toHaskellList test

prop_toHaskellList_1 :: NonEmptyList Int -> Bool
prop_toHaskellList_1 (NonEmpty lst) = toHaskellList (toList lst) == lst

toHaskellList_test = do
    quickCheck prop_toHaskellList_1

-- 15. append test

prop_append_1 :: [Int] -> [Int] -> Bool
prop_append_1 lst1 lst2 = 
    toHaskellList (toList lst1 `append` toList lst2) == lst1 ++ lst2

prop_append_2 :: [Int] -> [Int] -> Bool
prop_append_2 lst1 lst2 = 
    (reverse . toHaskellList) (toList lst1 `append` toList lst2) == 
        toHaskellList ((toList . reverse) lst2 `append` (toList . reverse) lst1)

prop_append_3 :: [Int] -> [Int] -> Bool
prop_append_3 lst1 lst2 = 
    length (toHaskellList $ toList lst1 `append` toList lst2) == 
        length lst1 + length lst2

append_test = do
    quickCheck prop_append_1
    quickCheck prop_append_2
    quickCheck prop_append_3

-- 16. removeAll test

prop_removeAll_1 :: (Int -> Bool) -> [Int] -> Bool
prop_removeAll_1 f lst =
    toHaskellList (removeAll f (removeAll f customList)) == 
        toHaskellList (removeAll f customList)
        where customList = toList lst

prop_removeAll_2 :: (Int -> Bool) -> [Int] -> Bool
prop_removeAll_2 f lst = 
    not $ any f $ toHaskellList $ removeAll f $ toList lst

prop_removeAll_3 :: [Int] -> Bool
prop_removeAll_3 lst = 
    toHaskellList (removeAll (\item -> True) $ toList lst) == []

removeAll_test = do
    quickCheck $ prop_removeAll_1 odd
    quickCheck $ prop_removeAll_1 even
    quickCheck $ prop_removeAll_1 is_bit
    quickCheck $ prop_removeAll_2 odd
    quickCheck $ prop_removeAll_2 even
    quickCheck $ prop_removeAll_2 is_bit
    quickCheck prop_removeAll_3

-- 17. sort test

prop_sort1 :: [Int] -> Bool
prop_sort1 lst = 
    toHaskellList (sort $ sort $ toList lst) == 
        toHaskellList (sort $ toList lst)

prop_sort2 :: [Int] -> Bool
prop_sort2 lst = 
    toHaskellList (sort $ toList lst) == 
        toHaskellList (sort $ toList $ reverse lst)

prop_sort3 :: Int -> Bool
prop_sort3 n = toHaskellList (sort $ toList [1..n]) == [1..n]

prop_sort4 :: [Int] -> Bool
prop_sort4 lst = length lst == length (toHaskellList $ sort $ toList lst)

sort_test = do
    quickCheck prop_sort1
    quickCheck prop_sort2
    quickCheck prop_sort3
    quickCheck prop_sort4

-- ============================================================================
-- Test All
-- ============================================================================

run_all_test = do
    putStrLn "\n===== Basic Functions Test ====="
    putStrLn "\nTesting snoc ..."
    snoc_test
    putStrLn "\nTesting myappend ..."
    myappend_test
    putStrLn "\nTesting myreverse ..."
    myreverse_test
    putStrLn "\nTesting count_emirps ..."
    count_emirps_test
    putStrLn "\nTesting biggest_sum ..."
    biggest_sum_test
    putStrLn "\nTesting greatest ..."
    greatest_test
    putStrLn "\n===== Basic Bits Test ====="
    putStrLn "\nTesting is_bit ..."
    is_bit_test
    putStrLn "\nTesting flip_bit ..."
    flip_bit_test
    putStrLn "\nTesting is_bit_seq ..."
    is_bit_seq_test
    putStrLn "\nTesting invert_bits ..."
    invert_bits_test
    putStrLn "\nTesting bit_count ..."
    bit_count_test
    putStrLn "\nTesting all_basic_bit_seqs ..."
    all_basic_bit_seqs_test
    putStrLn "\n===== Custom List Test ====="
    putStrLn "\nTesting toList ..."
    toList_test
    putStrLn "\nTesting toHaskelList ..."
    toHaskellList_test
    putStrLn "\nTesting append ..."
    append_test
    putStrLn "\nTesting removeAll ..."
    removeAll_test
    putStrLn "\nTesting sort ..."
    sort_test
