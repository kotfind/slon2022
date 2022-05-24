module Main(main) where

import Play
import Tree
import Ifio

tree_file :: FilePath
tree_file = "tree.dat"

main :: IO ()
main = do
    text <- readFile tree_file
    let t = parse $ filter (\l -> l /= "") $ lines text

    putStrLn "Game tree:"
    putStrLn $ show t

    t' <- play t 
    writeFile tree_file $ showRaw t'
    putStrLn "Great!\n"

    putStrLn $ replicate 40 '-'
    putStrLn "Would you like to play again? [y/n](д/н)"
    ifio main (putStrLn "Bye!")
