import Data.Char

--- Warm-up ---
--- 1 ---
any' :: (a -> Bool) -> [a] -> Bool
any' before = foldr ((||) . before) False

--- 2 ---
all' :: (a -> Bool) -> [a] -> Bool
all' compare = foldr (&&) True . map compare

--- 3 ---
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

--- 4 ---
cycle' :: [a] -> [a]
cycle' [] = []
cycle' xs = foldr recycle [] [1..]
   where recycle _ ctr = xs ++ ctr

--- 5 ---
scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f x xs = foldr scan [x] xs where 
    scan b a = (f b (head a)): a

--- 6 ---
scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' f x ys = reverse $ foldl (\a y -> (f (head a) y): a) [x] ys

--- 7 ---
inits :: [a] -> [[a]]
inits = foldr (\x y -> []: (map (x:) y)) [[]]

--- 8 ---
tails :: [a] -> [[a]]
tails = foldr (\x y -> reverse([]: reverse(map (x:) y))) [[]]

--- Trickier Exercises ---
--- 1 ---
minmax :: (Ord a) => [a] -> (a,a)
minmax (x:xs) = foldl (\(minimum, maximum) a -> (min minimum a, max maximum a)) (x,x) xs

--- 2 ---
gap :: Eq a => a -> a -> [a] -> Maybe Int
gap num1 num2 xs = case dropWhile (/= num1) xs of
    (_:xs) -> dist num2 xs
    [] -> Nothing
                      
dist num2 = foldr f Nothing
    where f x ctr | x == num2 = Just 1
                  | otherwise = (+1) <$> ctr

--- 3 --- 
evalExpr :: String -> Integer
evalExpr (x:xs) = (foldl (.) id (convert xs [])) (read [x]::Integer)
convert [] f = f
convert ('+':n:tail) f = convert tail (f++[(+(read [n]::Integer))])
convert ('-':n:tail) f = convert tail (f++[((read [n]::Integer)-)])

--- 4 ---
words' :: String -> [String]
words' = foldr (\x ctr -> if x == ' ' || head ctr == "" || (head $ head ctr) /= ' '  
    then (x: head ctr) : tail ctr
    else [x]: ctr) [""]

--- 5 ---
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' a = fst . foldr f ([],[]) where
    f bl (xs,ys) = (if a bl then xs else zs, zs) where zs = bl : ys

--- MP1 Redux ---
--- 4 ---

join :: a -> [[a]] -> [a]
join sep = foldl f [] where
    f [] a = a
    f a b = a ++ sep:b

--- 5 ---
unzip' :: [(a,b)] -> ([a], [b])
unzip' xs = foldr f x xs
    where
        f (a,b) (as,bs) = (a:as, b:bs)
        x = ([],[])

--- 6 ---
runLengthEncode :: String -> [(Int,Char)]
runLengthEncode = foldr runlengthencoder [] where
        runlengthencoder i [] = [(1,i)]
        runlengthencoder i x@((x1,x2):xs) | i == x2 = (x1+1,x2):xs
                                          | otherwise = (1,i):x

--- 7 ---
runLengthDecode :: [(Int,Char)] -> String
runLengthDecode (x:xs) = foldl (++) (decode x) (map decode xs)
decode (1, x) = x:[]
decode (c, x) = x: decode(c-1,x)

--- 8 ---
vigenere :: String -> String -> String
vigenere x [] = []
vigenere [] x = x
vigenere x (y:ys)
    | (abc < 26) = [sum] ++ vigenere (tail b) ys
    | otherwise = [ (['A'..'Z'] !! (abc - 26))] ++ vigenere (tail b) ys
    where a = if y `elem` ['.', ',', '?', '!', ':', ';', ' '] then ([' '] ++ x) else x
          b = take (rem(length a) (length(y:ys)) + 1) (a)
          c = toUpper (head b)
          d = toUpper y  
          abc = (ord(c) - 65 + ord(d) - 65)
          sum = if (abc < 0) || (abc == 3) then chr(ord(d)) else (['A'..'Z'] !! abc)      
