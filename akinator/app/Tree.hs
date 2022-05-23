module Tree
    ( Tree (Node, Leaf)
    , showRaw
    , parse) where

import Debug.Trace

data NodeType = YesNode | NoNode | NoTypeNode

instance Show NodeType where
    show YesNode = "\ESC[32m(Y)\ESC[0m "
    show NoNode = "\ESC[31m(N)\ESC[0m "
    show NoTypeNode = ""

data Tree =
      Node String Tree Tree
    | Leaf String

instance Show Tree where
    show t = show' t 0 NoTypeNode

show' :: Tree -> Int -> NodeType -> String
show' (Leaf s) d t =
       replicate (4 * d) ' '
    ++ show t
    ++ s
    ++ "\n"
show' (Node s l r) d t =
       replicate (4 * d) ' '
    ++ show t
    ++ s
    ++ "\n"
    ++ show' l (d + 1) YesNode
    ++ show' r (d + 1) NoNode

showRaw :: Tree -> String
showRaw t = showRaw' t 0

showRaw' :: Tree -> Int -> String
showRaw' (Leaf s) d =
       replicate (4 * d) ' '
    ++ s
    ++ "\n"
showRaw' (Node s l r) d =
       replicate (4 * d) ' '
    ++ s
    ++ "\n"
    ++ showRaw' l (d + 1)
    ++ showRaw' r (d + 1)

parse' :: [(Int, String)] -> (Tree, [(Int, String)])
parse' [(i, s)] = (Leaf s, [])
parse' ((i, s):ls)
    | i' <= i = (Leaf s, ls)
    | True    = (Node s l r, es)
      where
        (i', s') = head ls
        (l, rs) = parse' ls
        (r, es) = parse' rs

ind :: String -> (Int, String)
ind "" = (0, "")
ind (' ':l) = (i + 1, s)
    where (i, s) = ind l
ind ('\t':l) = (i + 4, s)
    where (i, s) = ind l
ind l = (0, l)

parse :: [String] -> Tree
parse ls = fst $ parse' $ ind <$> ls
