module Angabe6 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
   5. Ersetzen Sie die Trivialimplementierungen error "Nicht implementiert" durch
      sinnvolle Implementierungen, die die jeweilige Aufgabenstellung erfüllen.
-}


type Nat0        = Int
type Nat1        = Int
type Zeilenzahl  = Nat1
type Spaltenzahl = Nat1
type Zeile       = Nat1
type Spalte      = Nat1
type Skalar      = Int
type Matrixtyp   = (Zeilenzahl,Spaltenzahl)
type Matrixfkt   = Zeile -> Spalte -> Skalar  -- ausschliessl. total def. Abb.!

-- Matrizenwerte als Typ und funktionale Darstellung
data MatrixF = Mf { mtyp :: Matrixtyp, mf :: Matrixfkt }

-- Namesvereinbarung fuer den Fehlerwert
fehler = Mf (0,0) (\_ _ -> 0) :: MatrixF



-- Aufgabe A.1

instance Show MatrixF where
 show (Mf t f) =   "(" ++ (buildM 1 1 t f) ++ ")"


-- Aufruf: buildM 1 1 (2,2) f , wobei 1 1 die Startwerte 1 1 sind
buildM :: Int -> Int -> (Zeilenzahl,Spaltenzahl) -> (Nat1 -> Nat1 -> Int)  -> String
buildM x y tuple f
   | x == 1 && y == 1 && x == fst(tuple) && y == snd(tuple) = "[" ++ (show (mfkt f x y)) ++ "]" 
   | fst(tuple) == 0 || snd(tuple) == 0 = ""
   | y == 1 && x < fst(tuple) && y < snd(tuple) = "[" ++ (show (mfkt f x y)) ++ "," ++  (buildM x (y+1) tuple f)
   | y == 1 && x < fst(tuple) && y == snd(tuple) = (show (mfkt f x y)) ++ "] " ++  (buildM (x+1) 1 tuple f)
   | y == 1 && x == fst(tuple) && y < snd(tuple) = "[" ++ (show (mfkt f x y)) ++ ","  ++  (buildM x (y+1) tuple f)
   | y == 1 && x == fst(tuple) && y == snd(tuple) = (show (mfkt f x y)) ++ "]" 
   | x < fst(tuple) && y < snd(tuple) ||  x == fst(tuple) && y < snd(tuple)  = show (mfkt f x y) ++ ","  ++  (buildM x (y+1) tuple f)
   | x < fst(tuple) && y == snd(tuple) = (show (mfkt f x y)) ++ "] " ++  (buildM (x+1) 1 tuple f) 
   | x == fst(tuple) && y == snd(tuple) = (show (mfkt f x y)) ++ "]"
   | otherwise = ""
   where mfkt :: (Nat1 -> Nat1 -> Int) -> (Nat1 -> Nat1 -> Int)
         mfkt f = g where g = f 


--------------------------- Bsp
{-
mat = Mf (2,3) (\z s -> s)
m1 = Mf (2,2) (\z s -> if z == 1 then s else z+s) :: MatrixF
m2 = Mf (1,1) (\z s -> 1) :: MatrixF
m2 = Mf (2,2) (\z s -> s + ((z-1)*(snd (2,2)))) :: MatrixF
m3 = Mf (2,2) (\z s -> s + ((z-1)*(snd (mtyp m2)))) :: MatrixF
m4 = Mf (2,2) (\z s -> if z == 1 then (succ (fib (s-1))) else ((+) z (binom z (s-1)))) :: MatrixF
m5 = Mf (3,2) (\z s -> if z == 1 then s else if z == 2 then z+s else succ (z+s)) :: MatrixF
m6 = Mf (3,2) (\z s -> s + ((z-1)*(snd (mtyp m5)))) :: MatrixF
m7 = Mf (0,0) (\_ _ -> 0) :: MatrixF
m8 = Mf (0,0) (\z s -> z+s) :: MatrixF
m10 = Mf (2,3) (\z s -> 1) :: MatrixF
m11 = Mf (3,3) (\z s -> if (mod z 2 == 0) then 1 else -1 )

--------------------------- Hilfsfunktionen für Beispiele
fib :: Nat0 -> Nat0
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

binom :: Nat0 -> Nat0 -> Nat1
binom n k
   | n==0 || n==k = 1
   | True = binom (n-1) (k-1) + binom (n-1) k
-}


{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Show folgendermassen vor:
   ...
-}


-- Aufgabe A.2

matrixtyp :: MatrixF -> Maybe Matrixtyp
matrixtyp (Mf (x,y) f) 
   |  x == 0 || y == 0 = Nothing
   |  otherwise =  Just (x,y)


{- Knapp, aber gut nachvollziehbar geht natrixtyp folgendermassen vor:
   ...
-}



-- Aufgabe A.4

instance Eq MatrixF where
 mat1@(Mf t1 f1) == mat2@(Mf t2 f2)  
   |  (matrixtyp mat1 == Nothing) || (matrixtyp mat2 == Nothing) = error "Gleichheit undefiniert"
   |  t1 /= t2 || (show mat1 ) /= (show mat2) = False
   |  otherwise = True

 mat1@(Mf t1 f1) /= mat2@(Mf t2 f2)   
   |  (matrixtyp mat1 == Nothing) || (matrixtyp mat2 == Nothing) = error "Ungleichheit undefiniert"
   |  t1 /= t2 || (show mat1 ) /= (show mat2) = True
   |  otherwise = False

{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Eq folgendermassen vor:
   ...
-}



-- Aufgabe A.5
{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Num folgendermassen vor:
   ...
-}
instance Num MatrixF where
 mat1@(Mf t1 f1) + mat2@(Mf t2 f2)
   |  (matrixtyp mat1 == Nothing) || (matrixtyp mat2 == Nothing) || t1 /= t2 = fehler
   |  otherwise =  Mf t1 (\z s -> (f1 z s) + (f2 z s))

 mat1@(Mf t1 f1) - mat2@(Mf t2 f2) 
   |  (matrixtyp mat1 == Nothing) || (matrixtyp mat2 == Nothing) || t1 /= t2 = fehler
   |  otherwise =  Mf t1 (\z s -> (f1 z s) - (f2 z s))

 mat1@(Mf t1 f1) * mat2@(Mf t2 f2)
   |  (matrixtyp mat1 == Nothing) || (matrixtyp mat2 == Nothing) || snd(t1) /= fst(t2) = fehler
   |  otherwise = Mf (fst(t1), snd(t2)) (\z s -> sum( [(f1 z const) * (f2 const s) | const <- [1..snd(t1)]] ))
   
 negate mat1@(Mf t f)
   |  (matrixtyp mat1 == Nothing) = fehler
   |  otherwise =  Mf t (\z s -> (-1)*(f z s))

 abs mat1@(Mf t f) 
   |  (matrixtyp mat1 == Nothing) = fehler
   |  otherwise =  Mf t (\z s ->  abs (f z s))

 signum mat1@(Mf t f)       
   |  (matrixtyp mat1 == Nothing) = error "Vorzeichenfunktion undefiniert"
   |  (f 1 1) > 0  && (all (>0) asList) = Mf (1,1) (\z s -> 1)
   |  (f 1 1) < 0  && (all (<0) asList) = Mf (1,1) (\z s -> (-1)) 
   |  (f 1 1) == 0  && (all (==0) asList) = Mf (1,1) (\z s -> 0) 
   |  otherwise = error "Vorzeichenfunktion undefiniert"
   where asList = [(f z s) | z <- [1..(fst t)] , s <- [1..(snd t)]]

 fromInteger n           = Mf (1,1) (\z s -> fromIntegral n)

