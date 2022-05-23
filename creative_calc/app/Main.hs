module Main(main) where

import Control.Monad
import Text.Printf

max_val :: Int
max_val = 100

file :: FilePath
file = "calc.py"

ops :: [(Char, Float -> Float -> Float)]
ops =
    [ ('+', (+))
    , ('-', (-))
    , ('*', (*))
    , ('/', (/))
    ]

fmt :: String
fmt = "if x == '%d' and op == '%c' and y == '%d':\n    print('%f')\n"

int :: (Integral a, Num b) => a -> b
int = fromIntegral

main :: IO ()
main = do
    writeFile file "x, op, y = input().split()\n"
    forM_ [0..max_val] (\x -> do
        forM_ [0..max_val] (\y -> do
            forM_ ops (\(op, f) -> do
                    appendFile file $
                        printf fmt x op y (int x `f` int y)
                )
            )
        )
