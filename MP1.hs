--- CS 340
--- Machine Problem 1
--- Mert Armagan

import Data.Char

--- 1: CycleN ---
cycleN :: Int -> [a] -> [a]
cycleN _ [] = []
cycleN 0 xs = []
cycleN n xs = xs ++ cycleN (n-1) xs

--- 2: countLessThan ---
countLessThan :: (Ord a) => a -> [a] -> Int
countLessThan _ [] = 0
countLessThan x (y:ys) = if y < x then (1 + countLessThan x ys) else (0 + countLessThan x ys)

--- 3: removeAll ---
removeAll :: (Eq a) => [a] -> [a] -> [a]
removeAll _ [] = []
removeAll (x:xs) (y:ys)
                        | x == y    = removeAll [x] ys
                        | otherwise = y : removeAll [x] ys

--- 4: join ---
join :: a -> [[a]] -> [a]
join a [] = []
join a [x] = x
join a (x:xs) = x ++ a : (join a xs)

--- 5: unzip' ---
unzip' :: [(a,b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((a,b):xs) = (a:as, b:bs)
             where (as, bs) = unzip' xs

--- 6: runLengthEncode ---
runLengthEncode :: String -> [(Int, Char)]
runLengthEncode a = runlengthencoder 1 a
runlengthencoder _ []         = []
runlengthencoder i (b:[])     = [(i,b)]
runlengthencoder i (b1:b2:bs) | b1==b2 = runlengthencoder (i+1) (b2:bs)
                              | otherwise = (i,b1) : runlengthencoder 1 (b2:bs)                

--- 7: RunLengthDecode ---
runLengthDecode :: [(Int,Char)] -> String
runLengthDecode [] = []
runLengthDecode (x:xs) = decode x ++ runLengthDecode xs
decode (1, x) = x:[]
decode (c, x) = x: decode(c-1,x)

--- 8: vigenere ---
vigenere :: String -> String -> String
vigenere x [] = []
vigenere [] x = x
vigenere x (y:ys)
    | (abc < 26) = [sum] ++ vigenere (tail b) ys
    | otherwise = [ (['A'..'Z'] !! (abc - 26))] ++ vigenere (tail b) ys
    where a = if y `elem` ['.', ',', '?', '!', ':', ';', ' '] then ([' '] ++ x) else x
          b = cycleN (rem(length a) (length(y:ys)) + 1) (a)
          c = toUpper (head b)
          d = toUpper y  
          abc = (ord(c) - 65 + ord(d) - 65)
          sum = if (abc < 0) || (abc == 3) then chr(ord(d)) else (['A'..'Z'] !! abc)      