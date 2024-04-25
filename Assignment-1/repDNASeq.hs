-- Count the number of times that a given value occurs in a list (From lectures)
count x l = length (filter (==x) l)


-- Remove duplicate values from a list (From lectures)
rmDups [] = []
rmDups (x:xs) = x : filter (/= x) (rmDups xs)


-- Count occurences of unique elements (From lectures)
countOccurences vs = map (\x -> (count x vs, x)) (rmDups vs)


-- Return the substrings of length n preserving initial order
substringsOfLengthN :: String -> Int -> [String]
substringsOfLengthN str n
    | length str < n || n <= 0 = []
    | otherwise = takeWhile (\x -> length x == n) (map (\i -> take n (drop i str)) [0..])


-- Main function. Uses the helper functions to return the final result
-- Takes as input the length of the searched substrings
repDNASeq xs n = (map snd . filter (\(n, _) -> n > 1)) (countOccurences (substringsOfLengthN xs n))


-- -- Tests
-- main :: IO ()
-- main = do
--     print (substringsOfLengthN "" 3) -- []
--     print (substringsOfLengthN "a" 3) -- []
--     print (substringsOfLengthN "a" 0) -- []
--     print (substringsOfLengthN "abcdefg" 3) -- ["abc","bcd","cde","def","efg"]
--     print (substringsOfLengthN "abcdefg" 4) -- ["abcd","bcde","cdef","defg"]
--     print (substringsOfLengthN "abcdefg" 7) -- ["abcdefg"]

--     print (repDNASeq "AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT" 3) -- ["AAA","AAC","ACC","CCC","CCA","CAA"]
--     print (repDNASeq "AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT" 10) -- ["AAAAACCCCC","CCCCCAAAAA"]