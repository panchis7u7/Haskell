-- | int squared(int n){}
squared :: Integer -> Integer
squared n = n * 2

-- | bool inRange(int min. int max, int x){}
inRange :: Integer -> Integer -> Integer -> Bool
inRange min max x = x >= min && x <= max

-- | bool inRangeTwo(int min. int max, int x){} LazyEval
inRangeTwo :: Integer -> Integer -> Integer -> Bool
inRangeTwo min max x =
    let inLowerBound = min <= x
        inUpperBound = max >= x
    in
        inLowerBound && inUpperBound

-- | bool inRangeThree(int min. int max, int x){} LazyEval
inRangeThree :: Integer -> Integer -> Integer -> Bool
inRangeThree min max x = ilb && iub
    where
        ilb = min <= x
        iub = max >= x

-- | bool inRangeFour(int min. int max, int x){} LazyEval
inRangeFour :: Integer -> Integer -> Integer -> Bool
inRangeFour min max x =
    if ilb then iub else False
    where
        ilb = min <= x
        iub = max >= x

-- | Recursion with if then.
factorial:: Integer -> Integer
factorial n =
    if n <= 1 then
        1
    else
        n * factorial (n-1)

{- | Recursion with guards. Otherwise is a constant
    that always evaluates to True.-}
factorialGuards:: Integer -> Integer
factorialGuards n
    | n <= 1 = 1
    | otherwise = n * factorial (n-1)

{- | Recursion with Accumulators. La funcion factorialAccum
es el resultado de una funcion auxiliar aux, acc is the accumulator -}
factorialAccum:: Integer -> Integer
factorialAccum n = aux n 1
    where
        aux n acc
            | n <= 1 = acc
            | otherwise = aux (n-1) (n*acc)

fiboGuards:: Integer -> Integer
fiboGuards n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fiboGuards (n-1) + fiboGuards (n-2)

fiboAlt:: Integer -> Integer
fiboAlt 0 = 0
fiboAlt 1 = 1
fiboAlt n = fiboAlt(n-1) + fiboAlt(n-2)

{- Very fast fibonacci implementation. -}
fiboAcc :: Int -> Int
fiboAcc n = fib 0 1 n
    where fib a b n
              | n <= 1 = b
              | otherwise = fib b (a+b) (n-1)

{- # LANGUAGE BangPatterns # -XBangPatterns -}
fibfast :: Int -> Int
fibfast n = fib' 0 1 n
    where fib' a !b !n
              | n <= 1 = b
              | otherwise = fib' b (a+b) (n-1)

{- List creation. -}
createList:: Int -> Int -> [Int]
createList n m
    | m < n = []
    | m == n = [m]
    | otherwise = n : createList (n+1) m

{- import Data.List -}
{- Head Function -}
{- Tail Function -}
{- Length Function -}
{- Init Function -}
{- null function -}
{- and function -}
{- or function -}

{- List Comprehension. -}
{-[2*x | x <- [1,2,3]] => [2,4,6]-}
{-[2*x | x <- [1,2,3], x>1] => [4,6] -}

{- List Patterns -}

{- Sum list elements. -}
sumList:: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

{- Evens in list. -}
evens:: [Int] -> [Int]
evens [] = []
evens (x:xs)
          | mod x 2 == 0 = x : evens xs
          | otherwise = evens xs

evensSum = sumList . evens

elem:: (Eq a) => a -> [a] -> Bool
elem _ [] = False
elem e (x:xs) = (e == x) ||  (Main.elem e xs)

{- Create a function nub that removes all duplicates from a given list. -}
nub:: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs)
        | Main.elem x xs = nub xs
        | otherwise = x : nub xs

{- Create a function isAsc that returns True if the list given to it is a list
of ascending order. -}
isAsc:: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x:y:xs) =
    (x <= y) && isAsc (y:xs)

{- Create a function hasPath that determines if a path from one node to another
exists within a dircted graph. -}
hasPath:: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] x y = x == y
hasPath xs x y
            | x == y = True
            | otherwise =
                let xs' = [(n,m) | (n,m) <- xs, n /= x] in
                or [hasPath xs' m y | (n,m) <- xs, n == x]

{- Create a custom map function using Higher order functions. -}
customMap:: (a -> b) -> [a] -> [b]
customMap f [] = []
customMap f (x:xs) = f x : customMap f xs

{- (.):: (b -> c) -> (a -> b) -> a -> c -}
