sum' :: ( Num a ) => [a] -> a
sum' [] = 0
sum' ( x:xs ) = x + sum' xs

--wield :: [a] -> a
wield f = (head f) + (last f)

head' :: [a] -> String
head' [] = error "You are doing something wrong."
head' (x:_) = "The first one"

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty (x:_) = False

isBig :: (RealFloat a) => a -> Bool
isBig x
    | x > 100 = True
    | x <= 100 = False

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
    | a > b = GT
    | a < b = LT
    | a == b = EQ

isMeanBig :: (RealFloat a) => a -> a -> Bool
isMeanBig a b
    | mean > 100 = True
    | mean <= 100 = False
                    where mean = (a+b)/2

-- What is this thing??
calcBmis :: ( RealFloat a ) => [( a , a )] -> [ a ]
calcBmis xs = [ bmi w h | (w , h ) <- xs ]
    where bmi weight height = weight / height ^ 2

sphereVol :: Float -> Float
sphereVol r =
    let volume = 4/3*3.141592*r^3
    in volume

-- Case version of head'
head'' :: [a] -> a
head''  xs = case xs of [] -> error "Whatever you're doing."
                        (x:_) -> x

describeList :: [a] -> String
describeList xs = "This list is " ++ case xs of [] -> "empty."
                                                [x] -> "lonely."
                                                xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "This list is " ++ what xs
    where what [] = "empty."
          what [x] = "lonely."
          what xs = "a longer list."

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Whatever you've done (2)."
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
                  where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "Whatever you've done (2)."
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n == 0 = []
    | otherwise = (x:replicate' (n-1) x)

take' :: (Ord i, Num i) => i -> [a] -> [a]
take' _ [] = []
take' n (x:xsm)
    | n <= 0 = []
    | otherwise = (x:take' (n-1) xsm)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
