module PrettyAnsi where

ansiBlack :: [Char]
ansiBlack = "\x1b[30m"
ansiRed :: [Char]
ansiRed = "\x1b[31m"
ansiGreen :: [Char]
ansiGreen = "\x1b[32m"
ansiYellow :: [Char]
ansiYellow = "\x1b[33m"
ansiBlue :: [Char]
ansiBlue = "\x1b[34m"
ansiMagenta :: [Char]
ansiMagenta = "\x1b[35m"
ansiCyan :: [Char]
ansiCyan = "\x1b[36m"
ansiWhite :: [Char]
ansiWhite = "\x1b[37m"
ansiReset :: [Char]
ansiReset = "\x1b[0m"
ansiBold :: [Char]
ansiBold = "\x1b[0m"

prependColor :: [[Char]] -> Int -> (Int, [Char]) -> [Char]
prependColor colors colorsCount (number, word)= do
    let colorIndex = mod number colorsCount
        color =  colors !! colorIndex
    color ++ word


paintWords :: [[Char]] -> [[Char]] -> [[Char]]
paintWords words colors = do
    let mapper = prependColor colors $ length colors
    fmap mapper $ zip [0..] words

bold :: [Char] -> [Char]
bold string = ansiBold ++ string
