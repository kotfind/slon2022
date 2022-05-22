{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Data.List.Split (chunksOf)
import Safe.Exact (takeExact)

data Point = Point Int Int
sqhypot :: Point -> Point -> Int
sqhypot (Point x1 y1) (Point x2 y2) = (x1 - x2)^2 + (y1 - y2)^2

pairs :: (a -> a -> b) -> [a] -> [b]
pairs f [] = []
pairs f (x:xs) = (f x <$> xs) ++ pairs f xs

main :: IO ()
main = do
    content <- getContents
    let pts = takeExact 3 $
            (\[x, y] -> Point (read x) (read y)) <$>
                    chunksOf 2 (words content)

    let sqlns = pairs sqhypot pts
    let tmp = (\v -> sum sqlns - 2 * v) <$> sqlns

    putStrLn $ "The triangle is " ++ if
        | any (\v -> v < 0) tmp  -> "obtuse."
        | any (\v -> v == 0) tmp -> "right."
        | otherwise              -> "acute."
