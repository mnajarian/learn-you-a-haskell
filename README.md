# learn-you-a-haskell

Follows my work through *Learn You a Haskell for Great Good!* http://learnyouahaskell.com/chapters

The exercise prompts are from:  https://github.com/noelmarkham/learn-you-a-haskell-exercises


# BWT and suffix array implementation in Haskell

The Haskell file `bwt.hs` contains functionality to build a BWT and suffix array from an input string. 

To compile: `ghc --make bwt`

To generate a BWT, add as arguments a text file and an output file name for the BWT.
```
./bwt generateBWT testText.txt testBWT.txt
```

To generate the suffix array, specify the text file as an argument. The resulting suffix array will print to the terminal.
```
./bwt generateSuffixArray testText.txt
```
