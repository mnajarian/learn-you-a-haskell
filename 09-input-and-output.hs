import System.Environment
import System.Random

{-
 - Lets implement the UNIX echo command
 - The program arguments are simply printed to the standard output.
 - If the first argument is -n, this argument is not printed, and no trailing newline is printed
 -}

main = do
    args <- getArgs
    showArgs args

showArgs :: [String] -> IO ()
showArgs a 
    | (head a) == "-n" = return ()
    | otherwise = putStr $ unlines a


{- Write a lottery number picker
 - This function should take a StdGen instance, and produce a list of six unique numbers between 1 and 49, in numerical order
 -}
lottery :: StdGen -> [Int]
lottery gen = quicksort $ take 6 $ randomRs (1,49) gen 

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

