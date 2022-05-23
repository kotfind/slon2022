module Main(main) where

import Play
import Tree

tree_file :: FilePath
tree_file = "tree.dat"

main :: IO ()
main = do
    text <- readFile tree_file
    let t = parse $ filter (\l -> l /= "") $ lines text
    putStrLn "Game tree:"
    putStrLn $ show t
    putStrLn "\n"
    t' <- play t 
    putStrLn "Great!"
    writeFile tree_file $ showRaw t'
