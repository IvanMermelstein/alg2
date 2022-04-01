module Lib
    ( someFunc, pares, safediv, mezcla, moverIzq, moverDer, insertar, ins
    ) where

import Text.Show.Functions

someFunc :: IO ()
someFunc = putStrLn "someFunc"

pares [] = []
pares (x:xs) = if even x then x: pares xs
                         else pares xs

data Maybes a = Nothings | Justs a deriving Show
safediv :: Int -> Int -> Maybes Int
safediv _ 0 = Nothings
safediv x y = Justs (x `div` y)

data Nat = Zero | Suc Nat deriving Show

add n Zero = n
add n (Suc m) = Suc (add n m)

-- data Color4 = { red :: Int,
--                 green :: Int,
--                 blue :: Int} deriving Show
-- data Color3 = C3 Int Int Int deriving Show
-- data Color2 = C2 (Int,Int,Int) deriving Show
type Color = (Int , Int, Int)
mezcla :: Color -> Color -> Color
mezcla (a,b,c) (d,e,f)  =  (div (a+d) 2,div (b+e) 2,div (c+f) 2)

type Linea = ([Char], Int)

vacia :: Linea
vacia = ([], 0)

moverIzq :: Linea -> Linea
moverIzq (xs,0) = (xs,0)
moverIzq(xs,n) = (xs,n-1)

-- largo :: Linea -> Int
-- largo ([], _) = 0
-- largo ((x:xs), n) = 1 + (largo (xs, n))

moverDer :: Linea -> Linea
moverDer (xs, n) = if n < length xs then (xs, n + 1) else (xs, n) 

-- moverDerr (xs, n) = (xs, min (n+1) (length xs))

moverIni :: Linea -> Linea
moverIni(xs,n) = (xs,0)

moverFin :: Linea -> Linea
moverFin(xs,n) = (xs,length xs)

insertar :: Char -> Linea -> Linea
--insertar c ([],0) = ([c],1)
-- insertar c (xs,0) = (c:xs,1)
-- insertarL 0 a xs = a:xs
-- insertarL n a (x:xs) = 
-- insertar c ((x:xs),n) = insertar c (xs,n-1)

insertar c (cs, p) = (ins c p cs, p + 1)

ins c 0 cs = c:cs
ins c n (y:cs) = y : ins c (n-1) cs 


type Lineaa = (String, String)
-- (caracteres anteriores al puntero, caracteres siguientes al puntero)
-- (Reverse de la cadena original)
-- "Hola" -> ("oH","la")

vaciaa :: Lineaa
vaciaa = ([], [])

moverIzqq :: Lineaa -> Lineaa
moverIzqq ([], rs) = ([], rs)
moverIzqq (l:ls, rs) = (ls, l:rs)

moverDerr :: Lineaa -> Lineaa
moverDerr (ls, []) = (ls, [])
moverDerr (ls, r:rs) = (r:ls, rs)

insertarr :: Char -> Linea -> Linea
insertarr c (ls, rs) = (c:ls, rs)

moverInii :: Lineaa -> Lineaa
moverInii ([], rs) = ([], rs)
moverInii (ls, rs) = ([], (reverse ls) ++ rs)
-- moverInii (l:ls, rs) = moverInii (ls, l:rs) 