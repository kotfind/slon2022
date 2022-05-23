{-# LANGUAGE MultiWayIf #-} 

module Play(play) where

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

play :: Tree -> IO (Tree)
play t@(Leaf s) = do
    putStrLn $ "It's " ++ s ++ ", isn't it? [y/n](д/н)"
    ifio (return t)
      (do
        putStrLn $ "What's the right answer?"
        ans <- getLine
        putStrLn $ "What question differs " ++ s ++ " from " ++ ans ++ "?"
        question <- getLine
        putStrLn $ "How to answer \"" ++ question ++ "\" about " ++ ans ++ "? [y/n](д/н)"
        let tans = Leaf ans
        ifio (return $ Node question tans t) (return $ Node question t tans)
      )
play t@(Node s l r) = do
    putStrLn $ s ++ " [y/n](д/н)"
    ifio
      (do
        l' <- play l
        return (Node s l' r)
      )(do
        r' <- play r
        return (Node s l r')
      )
