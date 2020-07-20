index :: Eq a => a -> [a] -> Int
index x l = idx l 0
    where idx [] _ = error "Not found"
          idx (y:ys) i = if x /= y then idx ys (i+1) else i

index' :: Eq a => a -> [a] -> Int
index' x l = let (_, r) = foldl iter b l
             in case r of Nothing -> error "Not found"
                          Just i -> i
    where iter (i, Nothing) y = if x /= y then (i+1, Nothing) else (i+1, Just i)
          iter r _ = r  
          b = (0, Nothing)

index'' :: Eq a => a -> [a] -> Int
index'' x l = foldr iter (error "Not found") $ zip l [0..]
    where iter (y, i) r = if x == y then i else r

index''' :: Eq a => a -> Int -> [a] -> Int
index''' x n l = let (_, _, r) = foldl iter b l
                 in case r of Nothing -> error "Not found"
                              Just i -> i
    where iter (i, 1, Nothing) y = if x /= y then (i+1, n, Nothing)
                                   else (i+1, 1, Just i)
          iter (i, n, Nothing) y = if x /= y then (i+1, n, Nothing)
                                   else (i+1, n-1, Nothing)
          iter r _ = r
          b = (0, n, Nothing)