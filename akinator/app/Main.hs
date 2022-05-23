{-# LANGUAGE MultiWayIf #-}

module Main(main) where

import Tree
import Data.Char

ifio :: IO a -> IO a -> IO a
ifio yes no = do
    l <- getLine
    if l == "" then do
        putStrLn "Wrong input!"
        ifio yes no
    else do
        let c = toLower $ head l
        if
          | c `elem` ['y', 'д'] -> yes
          | c `elem` ['n', 'н'] -> no
          | otherwise -> do
                putStrLn "Wrong input!"
                ifio yes no

play :: Tree -> IO ()
play (Leaf s) = putStrLn s
play t@(Node s l r) = do
    putStrLn $ s ++ " [y/n](д/н)"
    ifio (play l) (play r)

main :: IO ()
main = do
    text <- readFile "tree.dat"
    let t = parse $ filter (\l -> l /= "") $ lines text
    putStrLn "Game tree:"
    putStrLn $ show t
    putStrLn "\n\n"
    play t 
