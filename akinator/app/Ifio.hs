{-# LANGUAGE MultiWayIf #-}

module Ifio(ifio) where

import Data.Char

ifio :: IO a -> IO a -> IO a
ifio yes no = do
    l <- getLine
    if l == "" then do
        putStrLn "\ESC[31mEmpty input!\ESC[0m"
        ifio yes no
    else do
        let c = toLower $ head l
        if
          | c `elem` ['y', 'д'] -> yes
          | c `elem` ['n', 'н'] -> no
          | otherwise -> do
                putStrLn "\ESC[31mWrong input!\ESC[0m"
                ifio yes no
