-- Implementation of a Burrows-Wheeler Transform (BWT) on a string

{-
The Burrows-Wheeler Transform (BWT) is a string compression technique. 
The BWT is a invertible permutation of any text that will compress well
if it exhibits redundancy. It is constructed by joining the predecessor
symbol of the sorted cyclical suffixes of a string. 
-}

import System.Environment
import System.IO

main = do 
    args <- getArgs 
    generateBWT args 


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

 
