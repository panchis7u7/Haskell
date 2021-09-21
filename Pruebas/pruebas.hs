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
