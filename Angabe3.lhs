> module Angabe3 where

1. Vervollstaendigen Sie gemaess Angabentext!
2. Vervollst�ndigen Sie auch die vorgegebenen Kommentaranf�nge!
3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!

> type Nat1      = Int
> type Zeile     = [Int]

Matrizen konzeptuell als Listen von Zeilen dargestellt:

> newtype Matrix = M [Zeile] 

> fehlerwert = M [] :: Matrix

> data Matrixtyp = Mat (Nat1,Nat1) | KeineMatrix deriving (Eq,Show)




Aufgabe A.1

> instance Show Matrix where
>  show (M []) = "()"
>  show (M m) = "(" ++ listToString result ++ ")"
>   where result = ["[" ++ zeig x  ++ "]"| x <- m]

> zeig :: [Int] -> [Char]
> zeig [x] = show(x)
> zeig (x:xs) =  show(x) ++ ","  ++ (zeig xs) 
> zeig [] = []

> listToString :: [[Char]] -> String
> listToString [x] = x ++ ""
> listToString (x:xs) = x ++ " " ++ listMiddle xs
> listMiddle :: [[Char]] -> String
> listMiddle [x] = x
> listMiddle (x:xs) =  x ++ " " ++ listMiddle xs
> listMiddle [] = ""


Knapp, aber gut nachvollziebar geht die Instanzdeklaration fuer Show folgendermassen vor: 


Knapp, aber gut nachvollziehbar geht matrixtyp folgendermassen vor: 
...
Aufgabe A.2

> matrixtyp :: Matrix -> Matrixtyp
> matrixtyp (M []) = KeineMatrix
> matrixtyp (M ([]:_)) = KeineMatrix
> matrixtyp (M mat) = if((fkt (head res) res) == True) then Mat (m,n) else KeineMatrix
>     where res = [length(x) | x <- mat]
>           m = length(res)
>           n = head res

> fkt :: Int -> [Int] -> Bool
> fkt vgl [] = True
> fkt vgl (x:xs) = (vgl == x) && fkt vgl xs

Kommentar: kürzer mit "wert = all(==(head res)) res"




Knapp, aber gut nachvollziebar geht die Instanzdeklaration fuer Eq folgendermassen vor:

Aufgabe A.3

> instance Eq Matrix where
>  (==) (M m1) (M m2) 
>   |   matrixtyp (M m1) == KeineMatrix || matrixtyp (M m2) == KeineMatrix  = error "Gleichheit undefiniert" 
>   |   (matrixtyp (M m1) == matrixtyp (M m2)) = compareInner m1 m2  
>   |   otherwise = False
>  (/=) (M m1) (M m2) 
>   |   matrixtyp (M m1) == KeineMatrix || matrixtyp (M m2) == KeineMatrix  = error "Ungleichheit undefiniert" 
>   |   otherwise = not(compareInner m1 m2)

> compareMat :: Matrix -> Matrix -> Bool  
> compareMat (M m1) (M m2) = (m1_n == m2_n) && (m1_m == m1_n)
>       where m1_m = head [length(x) | x <- m1]
>             m1_n = length [length(x) | x <- m1]
>             m2_m = head [length(x) | x <- m2]
>             m2_n = length [length(x) | x <- m2]

> compareInner :: [Zeile] -> [Zeile] -> Bool
> compareInner [m1] [m2] = m1 == m2 
> compareInner (m1:m1s) (m2:m2s) = (m1 == m2) && compareInner m1s m2s
> compareInner [] _  = False 
> compareInner _ [] = False






Knapp, aber gut nachvollziebar geht die Instanzdeklaration fuer Num folgendermassen vor: 
... 
Aufgabe A.4

> instance Num Matrix where
>  (+) (M m1) (M m2) 
>   |   (vgl == False) = fehlerwert
>   |   (vgl == True) = M (zipWith plus m1 m2)
>   |   otherwise = fehlerwert
>       where vgl = typeProof (M m1) (M m2)
>  (-) (M m1) (M m2) 
>   |   (vgl == False) = fehlerwert
>   |   (vgl == True) = M (zipWith minus m1 m2)
>   |   otherwise = fehlerwert
>       where vgl = typeProof (M m1) (M m2)
>  (*) (M m1) (M m2)
>   |  (matrixtyp (M m1) == KeineMatrix) || (matrixtyp (M m2) == KeineMatrix) == True = fehlerwert
>   |   (proof (m1) (m2)) = M (multipliziere m1 (transpose m2))
>   |   otherwise = fehlerwert
>  negate (M m1)
>   |   (matrixtyp (M m1) == KeineMatrix ) = fehlerwert
>   |   otherwise = M (neg m1)
>  abs (M m1)
>   |   (matrixtyp (M m1) == KeineMatrix ) = fehlerwert
>   |   otherwise = M (absMat m1)
>  signum (M m1)
>   |   (matrixtyp (M m1) == KeineMatrix ) = error "Vorzeichenfunktion undefiniert"
>   |   otherwise = matCheck m1
>  fromInteger n = M [[fromInteger n]]-- M [[fromInteger n]]

fromInteger n = intToMatrix (fromInteger n) -- M [[fromInteger n]]



> typeProof :: Matrix -> Matrix -> Bool 
> typeProof (M m1) (M m2) 
>   |   matrixtyp (M m1) == KeineMatrix = False
>   |   matrixtyp (M m2) == KeineMatrix = False
>   |   matrixtyp (M m1) == matrixtyp (M m2) = True
>   |   otherwise = False

> typCheck :: Matrixtyp -> Matrixtyp -> Bool 
> typeCheck m1 m2 = (m1 == KeineMatrix) && (m2 == KeineMatrix)
> typCheck (Mat (_,s)) (Mat (z,_)) = s==z

 typeCheck (Mat ()) _ = False
 typeCheck _ (Mat ()) = False

> proof :: [Zeile] -> [Zeile]  -> Bool
> proof m1 m2 = typCheck first second
>   where first = matrixtyp (M m1)
>         second = matrixtyp (M m2)

> plus :: [Int]->[Int]->[Int]
> plus x y = zipWith (+) x y

> minus :: [Int]->[Int]->[Int]
> minus x y = zipWith (-) x y



> multipliziere :: [Zeile]->[Zeile]->[Zeile]
> multipliziere [] _ = []
> multipliziere (x:xs) b = [(map (\y -> rowColAdd x y) b)] ++ (multipliziere xs b)
>   where rowColAdd :: Zeile -> Zeile -> Int
>         rowColAdd z1 z2 = sum (zipWith (*) z1 z2)






> transpose :: [Zeile] -> [Zeile]
> transpose ([]:_) = []
> transpose [[x]] = [[x]]
> transpose a = (map head a) : (transpose (map tail a))

> absMat :: [Zeile] -> [Zeile]
> absMat []  = []
> absMat (x:xs) = [(map (\y -> abs y) x)] ++ (absMat xs)

> neg :: [Zeile] -> [Zeile]
> neg []  = []
> neg (x:xs) = [(map (\y -> (-1) * y) x)] ++ (neg xs)

> skalar :: Int -> [Zeile] -> [Zeile]
> skalar ska []  = []
> skalar ska (x:xs) = [(map (\y -> ska * y) x)] ++ (skalar ska xs)

Fkt überprüft ersten Wert (neg=[-_]/null=[0]/pos=[+_]/sonst=[]) und bestimmt, ob Matrix konsistent positiv ist
-> vorher schon überprüfen, ob Matrix leer ist

> matCheck :: [Zeile] -> Matrix 
> matCheck mat 
>   |   all (all (< 0)) mat = M [[-1]]
>   |   (inkonsistent == True && ((head (headCheck mat) > 0) || (head (headCheck mat) < 0))) = error "Vorzeichenfunktion undefiniert"
>   |   (headCheck mat == [0] && nullCheck mat == True) = M [[0]]  
>   |   (headCheck mat == checkWert) == True = M [[1]]
>   |   otherwise = M [[-1]]
>    where inkonsistent = ((M mat) /= (abs (M mat)))
>          checkWert = (headCheck (absMat mat))
>          vglWert = [0] :: [Int]

> headCheck :: [Zeile] -> [Int]
> headCheck ([]:_) = []
> headCheck (x:xs) = firstOne x
>   where firstOne :: Zeile -> [Int]
>         firstOne (x:xs) = [x]


> nullCheck :: [Zeile] -> Bool 
> nullCheck ([]:_) = False 
> nullCheck(x:xs) = ((firstOne x) == 0)
>   where firstOne :: Zeile -> Int
>         firstOne (x:xs) = x

> intToMatrix :: Num a => a -> Matrix 
> intToMatrix a  = M [[2]] 

