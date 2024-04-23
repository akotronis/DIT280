int2List n = if n == 0 then [] else int2List (n `div` 10) ++ [n `mod` 10]
list2Int ls = foldl (\x -> \y -> 10*x + y) 0 ls
swapElementsAt i j xs = let elemI = xs !! i
                            elemJ = xs !! j
                            left = take i xs
                            middle = take (j - i - 1) (drop (i + 1) xs)
                            right = drop (j + 1) xs
                    in left ++ [elemJ] ++ middle ++ [elemI] ++ right

allSwaps (x:xs) = let swaps = [[swapElementsAt 0 i (x:xs)] | i <- [1..length xs]]
    in (x:xs) : concat swaps
    
maxSwap num =
    let numList = int2List num
        swaps = allSwaps numList
        maxNumList = foldl (\acc swap -> if list2Int swap > list2Int acc then swap else acc) numList swaps
        maxNum = list2Int maxNumList
    in maxNum

main :: IO ()
main = do 
  -- print (int2List 11)
  -- print (int2List 121)
  -- print (swapElementsAt 0 2 [1,2,3])
  -- print (list2Int [1,2,3])
  -- print (maxSwap 9973)
  -- print (maxSwap 1123)
  -- print (maxSwap 1222)
  print (allSwaps [1,2,3])