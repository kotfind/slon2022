{-# LANGUAGE MultiWayIf #-} 

module Play(play) where

import Tree
import Text.Printf
import Ifio

play :: Tree -> IO (Tree)
play t@(Leaf s) = do
    printf "It's \ESC[34m%s\ESC[0m, isn't it? [y/n](д/н)\n" s
    ifio (return t)
      (do
        putStrLn "What's the right answer?"
        ans <- getLine
        printf "What question differs \ESC[34m%s\ESC[0m from \ESC[34m%s\ESC[0m?\n" s ans
        question <- getLine
        printf "How to answer \"\ESC[34m%s\ESC[0m\" about \ESC[34m%s\ESC[0m? [y/n](д/н)\n" question ans
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
