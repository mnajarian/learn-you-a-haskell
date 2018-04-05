-- Implementation of a Burrows-Wheeler Transform (BWT) 
-- and suffix array on a string

{-
The Burrows-Wheeler Transform (BWT) is a string compression technique. 
The BWT is a invertible permutation of any text that will compress well
if it exhibits redundancy. It is constructed by joining the predecessor
symbol of the sorted cyclical suffixes of a string.

The suffix array is an array of indices in order of their lexicographically
sorted suffixes of a string.

-}

import System.Environment
import System.Directory
import System.IO
import Data.List

main = do 
    (command:args) <- getArgs 
    let (Just action) = lookup command dispatch
    action args 


dispatch :: [(String, [String] -> IO())]
dispatch = [ ("generateBWT", generateBWT)
           , ("generateSuffixArray", generateSuffixArray)
           ]


-- Generate a BWT from the string contained in fileIn and place it in 
-- a file called fileOut 
generateBWT :: [String] -> IO ()
generateBWT [] = return ()
generateBWT [fileIn, fileOut] = do
    putStr "Read file: "
    putStrLn fileIn
    contents <- readFile fileIn
    writeFile fileOut (stringToBWT contents)
    putStr "Wrote BWT file: "
    putStrLn fileOut


generateSuffixArray :: [String] -> IO ()
generateSuffixArray [] = return ()
generateSuffixArray [fileIn] = do 
    putStr "Read file: "
    putStrLn fileIn
    contents <- readFile fileIn
    -- writeFile fileOut (stringToSuffixArray contents)
    putStr "Suffix array: "
    print (stringToSuffixArray contents)



-- BWT FUNCTIONS

-- returns the cyclical suffix at index i of string s
-- if i is greater than (length(s)-1): returns original string
cyclicalSuffix :: Int -> String -> String
cyclicalSuffix i s = (drop i s) ++ (take i s)

-- returns all cyclical suffixes for a string
allCyclicalSuffixes :: [String] -> [String]
allCyclicalSuffixes [""] = [""]
allCyclicalSuffixes suffixes 
    | length suffixes == length (suffixes !! 0)  = suffixes
    | otherwise = 
        let newSuffixes = suffixes ++ [(cyclicalSuffix (length suffixes) (suffixes !! 0))]
        in allCyclicalSuffixes newSuffixes

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

-- returns the sorted cyclical suffixes for a string s 
sortedCyclicalSuffixes :: String -> [String]
sortedCyclicalSuffixes s = quicksort $ allCyclicalSuffixes [s]

-- return the BWT from a string s 
stringToBWT :: String -> String 
stringToBWT s = 
    let suffixes = sortedCyclicalSuffixes s  
    in foldl (\acc x -> acc ++ [(last x)]) "" suffixes


-- SUFFIX ARRAY FUNCTIONS

-- returns a tuple of (suffix, index) from string s at index i
suffixIndex :: String -> Int -> (String, Int)
suffixIndex s i = (drop i s, i)

-- returns a list of tuples (suffix, index) from a string s
-- with indices starting from 0 to i
-- note that to get all suffixes for a string, i = (length string)-1 
suffixIndices :: [(String,Int)] -> Int -> [(String,Int)]
suffixIndices suffixes i
    | length suffixes == length (fst $ suffixes !! 0) = suffixes 
    | otherwise = 
        let newSuffixes = suffixes ++ [suffixIndex (fst $ suffixes !! 0) i] 
        in suffixIndices newSuffixes (i-1)

-- returns the sorted list of (suffix,index) tuples
sortedSuffixIndices :: String -> [(String,Int)]
sortedSuffixIndices s = quicksort $ suffixIndices [(s, 0)] ((length s)-1)

-- returns the indices in suffix-sorted order
stringToSuffixArray :: String -> [Int]
stringToSuffixArray s = 
    let suffixes = sortedSuffixIndices s 
    in foldl (\acc x -> acc ++ [snd x]) [] suffixes

