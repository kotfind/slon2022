module Main(main) where

import Tree
import Data.Char

play :: Tree -> IO ()
play (Leaf s) = putStrLn s
play t@(Node s l r) = do
    putStrLn $ s ++ " [y/n](д/н)"

    (c':cs) <- getLine
    let c = toLower c'

    if c == 'y' || c == 'д'
        then play l
    else if c == 'n' || c == 'н'
        then play r
    else do
        putStrLn "Wrong input!"
        play t

main :: IO ()
main = do
    text <- readFile "tree.dat"
    let t = parse $ filter (\l -> l /= "") $ lines text
    putStrLn "Game tree:"
    putStrLn $ show t
    putStrLn "\n\n"
    play t 
