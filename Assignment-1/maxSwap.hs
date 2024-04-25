-- Convert an integer to a list
int2List n = if n == 0 then [] else int2List (n `div` 10) ++ [n `mod` 10]


-- Convert a list to an integer
list2Int ls = foldl (\x -> \y -> 10*x + y) 0 ls


-- Swap elements of a list xs in positions i and j and return the new list
swapElementsAt i j xs = let _i = min i j
                            _j = max i j
                            elemI = xs !! _i
                            elemJ = xs !! _j
                            left = take _i xs
                            middle = take (_j - _i - 1) (drop (_i + 1) xs)
                            right = drop (_j + 1) xs
                    in left ++ [elemJ] ++ middle ++ [elemI] ++ right


-- From an input list of integers return a list of lists with elements:
-- 1. The input list as first element if length of input list is > 1
-- 2. All the lists having swapped each element with the elements that follow
allSwaps :: [Int] -> [[Int]]
allSwaps [] = []
allSwaps [x] = [[x]]
allSwaps xs = xs : concat [concat [[swapElementsAt j i xs | i <- [j+1..length xs - 1]] | j <- [0..length xs - 2]]]
    

-- Main function. Uses the helper functions to return the final result
maxSwap num =
    let numList = int2List num
        swaps = allSwaps numList
        maxNumList = foldl (\inputNum swappedList -> if list2Int swappedList > list2Int inputNum then swappedList else inputNum) numList swaps
        maxNum = list2Int maxNumList
    in maxNum


-- -- Tests
-- main :: IO ()
-- main = do 
--   print (int2List 1) -- [1]
--   print (int2List 10) -- [1,0]
--   print (int2List 1234) -- [1,2,3,4]

--   print (list2Int [1]) -- 1
--   print (list2Int [1,0]) -- 10
--   print (list2Int [1,2,3,4]) -- 1234

--   print (swapElementsAt 0 1 [1,2]) -- [2,1]
--   print (swapElementsAt 1 2 [1,2,3,4]) -- [1,3,2,4]
--   print (swapElementsAt 3 1 [1,2,3,4]) -- [1,4,3,2]

--   print (allSwaps []) -- []
--   print (allSwaps [1]) -- [[1]]
--   print (allSwaps [1,2]) -- [[1,2],[2,1]]
--   print (allSwaps [1,2,3,4]) -- [[1,2,3,4],[2,1,3,4],[3,2,1,4],[4,2,3,1],[1,3,2,4],[1,4,3,2],[1,2,4,3]]

--   print (maxSwap 9973) -- 9973
--   print (maxSwap 1123) -- 3121
--   print (maxSwap 1222) -- 2221
--   print (maxSwap 4123) -- 4321
--   print (maxSwap 5413) -- 5431