module Angabe7 where


{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
   5. Ersetzen Sie die Trivialimplementierungen error "Nicht implementiert" durch
      sinnvolle Implementierungen, die die jeweilige Aufgabenstellung erfüllen.
   6. Kopieren Sie Ihre Implementierungen von Angabe 3 bzw. 6 an den
      entsprechenden Stellen ein. Beachten Sie, dass dafür drei Umbennennungen
      erforderlich sind, um Namenskonflikte zwischen Bezeichnungen von
      Angabe 3 und 6 zu vermeiden.
-}


type Nat0 = Int
type Nat1 = Int


-- Aufgabe A.1

-------------------------------------------------------------------------------- Aufgabe 3 wdh
-- Von Angabe 3 wiederholt:

-- Umbenennung 1: Matrixzeile von Angabe 3 wird fuer Angabe 7 umbenannt zu Matrixzeile
type Matrixzeile = [Int]

-- Matrizen konzeptuell als Listen von Matrixzeilen dargestellt:
newtype Matrix = M [Matrixzeile]  

fehlerwert = M [] :: Matrix

data Matrixtyp = Mat (Nat1,Nat1) | KeineMatrix deriving (Eq,Show)

--------------------------------------------------------------------------------instance: Show Matrix
instance Show Matrix where
   show (M []) = "()"
   show (M m) = "(" ++ listToString result ++ ")"
      where result = ["[" ++ zeig x  ++ "]"| x <- m]

zeig :: [Int] -> [Char]
zeig [x] = show(x)
zeig (x:xs) =  show(x) ++ ","  ++ (zeig xs) 
zeig [] = []

listToString :: [[Char]] -> String
listToString [x] = x ++ ""
listToString (x:xs) = x ++ " " ++ listMiddle xs
listMiddle :: [[Char]] -> String
listMiddle [x] = x
listMiddle (x:xs) =  x ++ " " ++ listMiddle xs
listMiddle [] = ""
--------------------------------------------------------------------------------


-------------------------------------------------------------------------------- fct: matrixtyp
matrixtyp :: Matrix -> Matrixtyp
matrixtyp (M []) = KeineMatrix
matrixtyp (M ([]:_)) = KeineMatrix
matrixtyp (M mat) = if((fkt (head res) res) == True) then Mat (m,n) else KeineMatrix
    where res = [length(x) | x <- mat]
          m = length(res)
          n = head res

fkt :: Int -> [Int] -> Bool
fkt vgl [] = True
fkt vgl (x:xs) = (vgl == x) && fkt vgl xs
--------------------------------------------------------------------------------

 
 -------------------------------------------------------------------------------- instance: Eq Matrix
instance Eq Matrix where
  (==) (M m1) (M m2) 
   |   matrixtyp (M m1) == KeineMatrix || matrixtyp (M m2) == KeineMatrix  = error "Gleichheit undefiniert" 
   |   (matrixtyp (M m1) == matrixtyp (M m2)) = compareInner m1 m2  
   |   otherwise = False
  (/=) (M m1) (M m2) 
   |   matrixtyp (M m1) == KeineMatrix || matrixtyp (M m2) == KeineMatrix  = error "Ungleichheit undefiniert" 
   |   otherwise = not(compareInner m1 m2)

compareMat :: Matrix -> Matrix -> Bool  
compareMat (M m1) (M m2) = (m1_n == m2_n) && (m1_m == m1_n)
      where m1_m = head [length(x) | x <- m1]
            m1_n = length [length(x) | x <- m1]
            m2_m = head [length(x) | x <- m2]
            m2_n = length [length(x) | x <- m2]

compareInner :: [Matrixzeile] -> [Matrixzeile] -> Bool
compareInner [m1] [m2] = m1 == m2 
compareInner (m1:m1s) (m2:m2s) = (m1 == m2) && compareInner m1s m2s
compareInner [] _  = False 
compareInner _ [] = False
--------------------------------------------------------------------------------



-------------------------------------------------------------------------------- instance: Num Matrix
instance Num Matrix where
  (+) (M m1) (M m2) 
   |   (vgl == False) = fehlerwert
   |   (vgl == True) = M (zipWith plus m1 m2)
   |   otherwise = fehlerwert
       where vgl = typeProof (M m1) (M m2)
  (-) (M m1) (M m2) 
   |   (vgl == False) = fehlerwert
   |   (vgl == True) = M (zipWith minus m1 m2)
   |   otherwise = fehlerwert
       where vgl = typeProof (M m1) (M m2)
  (*) (M m1) (M m2)
   |  (matrixtyp (M m1) == KeineMatrix) || (matrixtyp (M m2) == KeineMatrix) == True = fehlerwert
   |   (proof (m1) (m2)) = M (multipliziere m1 (transpose m2))
   |   otherwise = fehlerwert
  negate (M m1)
   |   (matrixtyp (M m1) == KeineMatrix ) = fehlerwert
   |   otherwise = M (neg m1)
  abs (M m1)
   |   (matrixtyp (M m1) == KeineMatrix ) = fehlerwert
   |   otherwise = M (absMat m1)
  signum (M m1)
   |   (matrixtyp (M m1) == KeineMatrix ) = error "Vorzeichenfunktion undefiniert"
   |   otherwise = matCheck m1
  fromInteger n = M [[fromInteger n]]




------------------------------------------ Hilfsfunktionen für "instance: Num Matrix"
typeProof :: Matrix -> Matrix -> Bool 
typeProof (M m1) (M m2) 
  |   matrixtyp (M m1) == KeineMatrix = False
  |   matrixtyp (M m2) == KeineMatrix = False
  |   matrixtyp (M m1) == matrixtyp (M m2) = True
  |   otherwise = False

typCheck :: Matrixtyp -> Matrixtyp -> Bool 
typeCheck m1 m2 = (m1 == KeineMatrix) && (m2 == KeineMatrix)
typCheck (Mat (_,s)) (Mat (z,_)) = s==z


proof :: [Matrixzeile] -> [Matrixzeile]  -> Bool
proof m1 m2 = typCheck first second
  where first = matrixtyp (M m1)
        second = matrixtyp (M m2)

plus :: [Int]->[Int]->[Int]
plus x y = zipWith (+) x y

minus :: [Int]->[Int]->[Int]
minus x y = zipWith (-) x y



multipliziere :: [Matrixzeile]->[Matrixzeile]->[Matrixzeile]
multipliziere [] _ = []
multipliziere (x:xs) b = [(map (\y -> rowColAdd x y) b)] ++ (multipliziere xs b)
  where rowColAdd :: Matrixzeile -> Matrixzeile -> Int
        rowColAdd z1 z2 = sum (zipWith (*) z1 z2)


transpose :: [Matrixzeile] -> [Matrixzeile]
transpose ([]:_) = []
transpose [[x]] = [[x]]
transpose a = (map head a) : (transpose (map tail a))

absMat :: [Matrixzeile] -> [Matrixzeile]
absMat []  = []
absMat (x:xs) = [(map (\y -> abs y) x)] ++ (absMat xs)

neg :: [Matrixzeile] -> [Matrixzeile]
neg []  = []
neg (x:xs) = [(map (\y -> (-1) * y) x)] ++ (neg xs)

skalar :: Int -> [Matrixzeile] -> [Matrixzeile]
skalar ska []  = []
skalar ska (x:xs) = [(map (\y -> ska * y) x)] ++ (skalar ska xs)


matCheck :: [Matrixzeile] -> Matrix 
matCheck mat 
  |   all (all (< 0)) mat = M [[-1]]
  |   (inkonsistent == True && ((head (headCheck mat) > 0) || (head (headCheck mat) < 0))) = error "Vorzeichenfunktion undefiniert"
  |   (headCheck mat == [0] && nullCheck mat == True) = M [[0]]  
  |   (headCheck mat == checkWert) == True = M [[1]]
  |   otherwise = M [[-1]]
   where inkonsistent = ((M mat) /= (abs (M mat)))
         checkWert = (headCheck (absMat mat))
         vglWert = [0] :: [Int]

headCheck :: [Matrixzeile] -> [Int]
headCheck ([]:_) = []
headCheck (x:xs) = firstOne x
  where firstOne :: Matrixzeile -> [Int]
        firstOne (x:xs) = [x]

nullCheck :: [Matrixzeile] -> Bool 
nullCheck ([]:_) = False 
nullCheck(x:xs) = ((firstOne x) == 0)
  where firstOne :: Matrixzeile -> Int
        firstOne (x:xs) = x

intToMatrix :: Num a => a -> Matrix 
intToMatrix a  = M [[2]] 

--------------------------------------------------------------------------------



-------------------------------------------------------------------------------- Aufgabe 6 wdh
-- Von Angabe 6 wiederholt:

type Zeilenzahl        = Nat1
type Spaltenzahl       = Nat1
type Zeile             = Nat1
type Spalte            = Nat1
type Skalar            = Int

-------------------------------------------------------------------------------- Definitionen
-- Umbenennung 2: Matrixtyp von Angabe 6 wird fuer Angabe 7 umbenannt zu Matrixtyp'
type Matrixtyp'  = (Zeilenzahl,Spaltenzahl)
type Matrixfkt   = Zeile -> Spalte -> Skalar  -- ausschliessl. total def. Abb.!

-- Matrizenwerte als Typ und funktionale Darstellung
data MatrixF = Mf { mtyp :: Matrixtyp', mf :: Matrixfkt }

-- Namesvereinbarung fuer den Fehlerwert des Typs MatrixF
fehler = Mf (0,0) (\_ _ -> 0) :: MatrixF

-------------------------------------------------------------------------------- instance: Show MatrixF
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
--------------------------------------------------------------------------------



-------------------------------------------------------------------------------- fct: matrixtyp'
-- Umbenennung 3: matrixtyp von Angabe 6 wird fuer Angabe 7 umbenannt zu matrixtyp'
matrixtyp' :: MatrixF -> Maybe Matrixtyp'
matrixtyp' (Mf (x,y) f) 
   |  x == 0 || y == 0 = Nothing
   |  otherwise =  Just (x,y)
--------------------------------------------------------------------------------


-------------------------------------------------------------------------------- instance: Eq
instance Eq MatrixF where
 mat1@(Mf t1 f1) == mat2@(Mf t2 f2)  
   |  (matrixtyp' mat1 == Nothing) || (matrixtyp' mat2 == Nothing) = error "Gleichheit undefiniert"
   |  t1 /= t2 || (show mat1 ) /= (show mat2) = False
   |  otherwise = True

 mat1@(Mf t1 f1) /= mat2@(Mf t2 f2)   
   |  (matrixtyp' mat1 == Nothing) || (matrixtyp' mat2 == Nothing) = error "Ungleichheit undefiniert"
   |  t1 /= t2 || (show mat1 ) /= (show mat2) = True
   |  otherwise = False

--------------------------------------------------------------------------------

-------------------------------------------------------------------------------- instance: Num
instance Num MatrixF where
 mat1@(Mf t1 f1) + mat2@(Mf t2 f2)
   |  (matrixtyp' mat1 == Nothing) || (matrixtyp' mat2 == Nothing) || t1 /= t2 = fehler
   |  otherwise =  Mf t1 (\z s -> (f1 z s) + (f2 z s))

 mat1@(Mf t1 f1) - mat2@(Mf t2 f2) 
   |  (matrixtyp' mat1 == Nothing) || (matrixtyp' mat2 == Nothing) || t1 /= t2 = fehler
   |  otherwise =  Mf t1 (\z s -> (f1 z s) - (f2 z s))

 mat1@(Mf t1 f1) * mat2@(Mf t2 f2)
   |  (matrixtyp' mat1 == Nothing) || (matrixtyp' mat2 == Nothing) || snd(t1) /= fst(t2) = fehler
   |  otherwise = Mf (fst(t1), snd(t2)) (\z s -> sum( [(f1 z const) * (f2 const s) | const <- [1..snd(t1)]] ))
   
 negate mat1@(Mf t f)
   |  (matrixtyp' mat1 == Nothing) = fehler
   |  otherwise =  Mf t (\z s -> (-1)*(f z s))

 abs mat1@(Mf t f) 
   |  (matrixtyp' mat1 == Nothing) = fehler
   |  otherwise =  Mf t (\z s ->  abs (f z s))

 signum mat1@(Mf t f)       
   |  (matrixtyp' mat1 == Nothing) = error "Vorzeichenfunktion undefiniert"
   |  (f 1 1) > 0  && (all (>0) asList) = Mf (1,1) (\z s -> 1)
   |  (f 1 1) < 0  && (all (<0) asList) = Mf (1,1) (\z s -> (-1)) 
   |  (f 1 1) == 0  && (all (==0) asList) = Mf (1,1) (\z s -> 0) 
   |  otherwise = error "Vorzeichenfunktion undefiniert"
   where asList = [(f z s) | z <- [1..(fst t)] , s <- [1..(snd t)]]

 fromInteger n           = Mf (1,1) (\z s -> fromIntegral n)
--------------------------------------------------------------------------------







-- Aufgabe A.2

class (Eq a,Num a,Show a) => MatrixTyp a where
 madd, msub, mmult :: a -> a -> a
 msmult  :: Int -> a -> a
 mtransp :: a -> a
 mdet    :: a -> Maybe Int
 mfehler :: a

 -- Protoimplementierungen
 madd  = (+)
 mmult = (*)
 msub  = (-)

-------------------------------------------------------------------------------- instance Matrix
instance MatrixTyp Matrix where
 msmult n (M zs)
   |  (matrixtyp (M zs) == KeineMatrix ) = fehlerwert
   |  otherwise =  (M (skalarM zs n))
 mtransp (M zs)
   |  (matrixtyp (M zs) == KeineMatrix ) = fehlerwert 
   |  otherwise = M (transpose zs)
 mdet (M zs) 
   |  (matrixtyp (M zs) == KeineMatrix ) = Nothing -- hier noch
   |  otherwise = Nothing
 mfehler = fehlerwert

---------------------------------------------------Hilfsfkt
skalarM :: [Matrixzeile] -> Int -> [Matrixzeile]
skalarM [] _ = []
skalarM (x:xs) n = [inner x n] ++ (skalarM xs n) 
  where inner :: Matrixzeile -> Int -> Matrixzeile
        inner matZ n = map (*n) matZ

isEqual :: Matrixtyp -> Bool
isEqual (Mat (x,y)) = (x == y)



-------------------------------------------------------------------------------- instance MatrixF
instance MatrixTyp MatrixF where
 msmult n mat1@(Mf t f) 
   |  (matrixtyp' mat1 == Nothing) = fehler
   |  otherwise =  Mf t (\z s -> (f z s) * n)
 mdet (Mf t f)     = error "Nicht implementiert!"
 mtransp mat1@(Mf t f) 
   |  (matrixtyp' mat1 == Nothing) = fehler
   |  otherwise =  Mf t (\z s -> (f s z))
 mfehler = fehler


{- Knapp, aber gut nachvollziehbar gehen die Instanzbildungen fuer 
   MatrixTyp folgendermassen vor:
   ...
-}

--fct1 :: [Matrixzeile] -> Int -> [Matrixzeile]
--fct1 [] _ = []
--fct1 (x:xs) n = head ++ (fct1 xs n)
--   where head = [x!!n] :: Matrixtyp






-- Aufgabe A.3

konv1 :: Matrix -> MatrixF
konv1 mat@(M zs) 
   | matrixtyp mat /= KeineMatrix  = Mf ((\(Mat x) -> x) (matrixtyp mat)) (\z s -> getValByInd z s mat)
   | otherwise = fehler

getValByInd :: Nat1 -> Nat1 -> Matrix -> Int 
getValByInd 0 _ _ = error "zeile ungültig"
getValByInd _ 0 _ = error "spalte ungültig"
getValByInd z s (M mat)
   |  (matrixtyp (M mat) == KeineMatrix) = error "matrix ungültig"
   |  z > dimension || s > dimension = mat!!(mod (z-1) dimension)!!(mod (s-1) dimension) -- als total definierte Fkt
   |  otherwise =  mat!!(z-1)!!(s-1)
    where dimension = (\(Mat (x,y)) -> x) (matrixtyp (M mat)) 

konv2 :: MatrixF -> Matrix
konv2 matF@(Mf (z,s) f) 
   |  matrixtyp' matF /= Nothing = M ( map (\x -> [f x y | y <- [1..s]]) [1..z]  )
   |  otherwise = fehlerwert



{- Knapp, aber gut nachvollziehbar gehen die Konvertierungsfunktionen 
   konv1 und konv2 folgendermassen vor:
   ...
-}



-- Aufgabe A.6


type Konservenzahl = Nat0

type Bahnhof     = String
type Ausgangsbhf = Bahnhof
type Zielbhf     = Bahnhof
type Von         = Bahnhof
type Nach        = Bahnhof

data VHDS   = Viertel | Halb | Dreiviertel | Schlag 
               deriving (Eq,Ord,Enum,Show)
data Stunde = I | II | III | IV | V | VI | VII | VIII | IX | X
              | XI | XII | XIII | XIV |  XV | XVI | XVII | XVIII 
              | XIX | XX | XXI | XXII | XXIII | XXIV 
               deriving (Eq,Ord,Enum,Show)

data Abfahrtzeit = AZ { vds :: VHDS, std :: Stunde } deriving (Eq,Ord,Show)
data Reisedauer  = RD { vs :: Stunde, bt :: VHDS } deriving (Eq,Ord,Show)

type Fahrplan = [(Abfahrtzeit,Von,Nach,Reisedauer)]


konservenrechner :: Fahrplan -> Ausgangsbhf -> Zielbhf -> Maybe Konservenzahl
konservenrechner fp ab zb = error "Nicht implementiert!"

{- Knapp, aber gut nachvollziehbar geht die Funktion konservenrechner 
   folgendermassen vor:
   ...
-}
