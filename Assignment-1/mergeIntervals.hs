-- Sort an input list according to the first integer element of its list elements
sortByStart :: [(Int, Int)] -> [(Int, Int)]
sortByStart [] = []
-- Use recursion to sort separatelly the sublists of rest of ipnut list elements with 
-- 1. First element <= that of x
-- 2. First element > that of x
sortByStart (x:xs) = sortByStart [y | y <- xs, fst y <= fst x] ++ [x] ++ sortByStart [y | y <- xs, fst y > fst x]


-- Helper class that accepts the sorted input from sortByStart and 
-- constructs the new merged intervals comparing endpoints and using recursion
mergeConstructor :: [(Int, Int)] -> [(Int, Int)]
mergeConstructor [] = []
mergeConstructor [x] = [x]
mergeConstructor ((start1, end1):(start2, end2):rest)
    | end1 >= start2 = mergeConstructor ((start1, max end1 end2) : rest)
    | otherwise = (start1, end1) : mergeConstructor ((start2, end2) : rest)


-- Main function. Uses the helper functions to return the final result
mergeIntervals :: [(Int, Int)] -> [(Int, Int)]
mergeIntervals intervals = mergeConstructor (sortByStart intervals)

-- -- Tests
-- main :: IO ()
-- main = do
--     print (sortByStart [])-- []
--     print (sortByStart [(1,1)]) -- [(1,1)]
--     print (sortByStart [(2,2), (1,1)]) -- [(1,1),(2,2)]
--     print (sortByStart [(3,4), (1,5), (7,10), (2,3), (6,8)]) -- [(1,5),(2,3),(3,4),(6,8),(7,10)]
--     print (sortByStart [(6,9),(2,8),(2,3),(13,13),(15,16),(1,2),(10,11)]) -- [(1,2),(2,3),(2,8),(6,9),(10,11),(13,13),(15,16)]

--     print (mergeConstructor [])-- []
--     print (mergeConstructor [(1,1)]) -- [(1,1)]
--     print (mergeConstructor [(1,1),(2,2)]) -- [(1,1),(2,2)]
--     print (mergeConstructor [(1,5),(2,3),(3,4),(6,8),(7,10)]) -- [(1,5),(6,10))]
--     print (mergeConstructor [(1,2),(2,3),(2,8),(6,9),(10,11),(13,13),(15,16)]) -- [(1,9),(10,11),(13,13),(15,16)]

--     print (mergeIntervals []) -- []
--     print (mergeIntervals [(1,1)]) -- [(1,1)]
--     print (mergeIntervals [(1,1), (2,2)]) -- [(1,1),(2,2)]
--     print (mergeIntervals [(3,4), (1,5), (7,10), (2,3), (6,8)]) -- [(1,5),(6,10)]
--     print (mergeIntervals [(1,2),(2,8),(2,3),(6,9),(10,11),(13,13),(15,16)]) -- [(1,9),(10,11),(13,13),(15,16)]