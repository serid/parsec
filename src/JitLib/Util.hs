module JitLib.Util where

-- foldMap-s arguments while inserting @sep@ between elements
foldMapSep :: [b] -> (a -> [b]) -> [a] -> [b]
foldMapSep sep f (x:xs) = (f x) ++ foldMap (\v -> sep ++ f v) xs
foldMapSep _ _ [] = []

-- Turns
-- [5, 5, 4, 3] into
-- [5, 50, 400, 3000]
powerList10 :: Int -> [Int] -> [Int]
powerList10 s (x:xs) = (x * (10 `power` s)):(powerList10 (succ s) xs)
powerList10 _ [] = []

-- Natural power function
power :: Int -> Int -> Int
power base p = coPower `iterate` 1 !! p where
    coPower :: Int -> Int
    coPower = \n -> n * base
