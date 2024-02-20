module Angabe5 where
import Data.List

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}


type Nat0 = Int


-- Die selbstdefinierte Typklasse Menge_von:

class Eq a => Menge_von a where
 leer          :: [] a
 vereinige     :: [] a -> [] a -> [] a     
 schneide      :: [] a -> [] a -> [] a
 ziehe_ab      :: [] a -> [] a -> [] a
 ist_teilmenge :: [] a -> [] a -> Bool
 ist_obermenge :: [] a -> [] a -> Bool
 ist_element   :: a -> [] a -> Bool
 ist_leer      :: [] a -> Bool
 sind_gleich   :: [] a -> [] a -> Bool
 anzahl        :: a -> [] a -> Nat0
 
 -- Protoimplementierungen
 leer = []
 vereinige xs ys     = xs ++ ys
 ist_teilmenge xs ys = ist_obermenge ys xs
 ist_obermenge xs ys = ist_teilmenge ys xs
 ist_element x xs    = anzahl x xs >= 1
 ist_leer xs         = xs == leer
 sind_gleich xs ys   = ist_teilmenge xs ys && ist_teilmenge ys xs


-- Weitere Typen:

newtype Paar a b = P (a,b) deriving (Eq,Show)

data Zahlraum_0_10 = N | I | II | III | IV | V | VI
                     | VII | VIII | IX | X | F deriving (Eq,Ord,Show, Bounded, Enum) 

newtype Funktion = Fkt { f :: Zahlraum_0_10 -> Zahlraum_0_10 } 

data Baum a = Blatt a | Knoten (Baum a) a (Baum a) deriving (Eq,Show)

newtype ElemTyp a = ET a

-- Pseudoheterogene Elementtypen
data PH_ElemTyp a b c d e = A a | B b | C c | D d | E e deriving (Eq,Show)
data PH_ElemTyp' q r s    = Q q | R r | S s deriving (Eq,Show)




-- Aufgabe A.1

instance Num Zahlraum_0_10 where
   (+) a b
      | (a /= F) && (b /= F) = z 
      | otherwise = F
      where x = getIndex N a 0 
            y = getIndex N b 0 
            z = getZahlraum (x+y) N
   (-) a b 
      | (a /= F) && (b /= F) = fromIntegral $ toInteger $ fromEnum a - fromEnum b
      | otherwise = F
      where x = fromEnum a
            y = fromEnum b
            z = getZahlraum (x-y) N
   (*) a b 
      | (a /= F) && (b /= F) = z 
      | otherwise = F
      where x = getIndex N a 0 
            y = getIndex N b 0 
            z = getZahlraum (x*y) N
   
   abs a = a
   signum a
      | a == F = F
      | a == N = N
      | otherwise = I
   fromInteger n
      | n < 0 || 10 < n = F 
      | otherwise = toEnum (fromIntegral n) :: Zahlraum_0_10

      




-- Aufruf: getIndex startwert=N suchwert startInt=0
-- leichter mit "fromEnum II = 2"
getIndex :: Zahlraum_0_10 -> Zahlraum_0_10 -> Int -> Int
getIndex startwert z counter
   | z == F = -1
   | z == startwert = counter
   | z /= startwert && z <= X = getIndex (succ startwert) z (counter+1)
   | otherwise = -1

-- Aufruf: getZahlraum x N
-- leichter mit "toEnum 2 :: Zahlraum_0_10 = 2"
getZahlraum :: Int -> Zahlraum_0_10 -> Zahlraum_0_10
getZahlraum  z startwert
   | z == -1 = F
   | z /= -1 && z == 0 = startwert
   | z /= -1 && z /= 0 && z < 12 = getZahlraum (z-1) (succ startwert)
   | otherwise = F   

{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Num folgendermassen vor:
   ...
-}




-- Aufgabe A.2
--------------------------------- Beispiel-Variablen

-- Aufruf: (Fkt fct1) == (Fkt fct2)
instance Eq Funktion where
   (==) (Fkt f1) (Fkt f2) = (map f1 (enumFrom N)) == map f2 (enumFrom N)

-- Aufruf: show test1
instance Show Funktion where
  show (Fkt f1) =  "{" ++ (conCat [ (x, f1 x) |   x <- (enumFrom N)]) ++ "}"


-- GEDANKE: erste Fkt -> zweite Fkt -> alle Werte einsetzen -> Bool
fctCheck :: Funktion -> Funktion -> Zahlraum_0_10 -> Bool
fctCheck (Fkt f1) (Fkt f2) startwert
   | (f1 startwert) == (f2 startwert) && startwert < X = fctCheck  (Fkt f1)  (Fkt f2) (succ startwert)
   | (f1 startwert) == (f2 startwert) && startwert == X = True 
   | otherwise = False

conCat :: Show a => [a] -> String
conCat [] = ""
conCat [x] = show x
conCat (x:xs) = show x ++ "," ++ conCat xs



{- Knapp, aber gut nachvollziehbar gehen die beiden Instanzbildungen fuer
   Eq und Show folgendermassen vor:
   ...
-}




-- Aufgabe A.3


instance Menge_von Int where
   vereinige as bs
      | (noDub as as) && (noDub bs bs) = ((delDubl as bs) ++ bs)
      | otherwise = error "Fehler"
   schneide as bs
      | (noDub as as) && (noDub bs bs) = onlyDub as bs
      | otherwise = error "Fehler"
   ziehe_ab as bs
      | (noDub as as) && (noDub bs bs) = [x | x <- as, not(elem x bs)]
      | otherwise = error "Fehler"   

   ist_teilmenge as bs
      | (noDub as as) && (noDub bs bs) = length(onlyDub as bs) == length(as)
      | otherwise = error "Fehler" 
   ist_leer as
      | (noDub as as) = length(as) == 0
      | otherwise = error "Fehler" 
   anzahl x as 
      | (noDub as as) = (nTimesFound2 x as) 
      | otherwise = error "Fehler" 
  

instance Menge_von Zahlraum_0_10 where
   vereinige as bs
      | (noDub as as) && (noDub bs bs) = ((delDubl as bs) ++ bs)
      | otherwise = error "Fehler"
   schneide as bs
      | (noDub as as) && (noDub bs bs) = onlyDub as bs
      | otherwise = error "Fehler"
   ziehe_ab as bs
      | (noDub as as) && (noDub bs bs) = [x | x <- as, not(elem x bs)]
      | otherwise = error "Fehler"   

   ist_teilmenge as bs
      | (noDub as as) && (noDub bs bs) = length(onlyDub as bs) == length(as)
      | otherwise = error "Fehler" 
   ist_leer as
      | (noDub as as) = length(as) == 0
      | otherwise = error "Fehler" 
   anzahl x as 
      | (noDub as as) = (nTimesFound2 x as) 
      | otherwise = error "Fehler" 


instance Menge_von Funktion where
   vereinige as bs
      | (noDub as as) && (noDub bs bs) = ((delDubl as bs) ++ bs)
      | otherwise = error "Fehler"
   schneide as bs
      | (noDub as as) && (noDub bs bs) = onlyDub as bs
      | otherwise = error "Fehler"
   ziehe_ab as bs
      | (noDub as as) && (noDub bs bs) = [x | x <- as, not(elem x bs)]
      | otherwise = error "Fehler"   

   ist_teilmenge as bs
      | (noDub as as) && (noDub bs bs) = length(onlyDub as bs) == length(as)
      | otherwise = error "Fehler" 
   ist_leer as
      | (noDub as as) = length(as) == 0
      | otherwise = error "Fehler" 
   anzahl x as 
      | (noDub as as) = (nTimesFound2 x as) 
      | otherwise = error "Fehler" 


------ vereinige
-- takes same list twice an checks each param wheter to be in twice in list
noDub :: Eq a => [a]-> [a] -> Bool
noDub [] [] = True 
noDub [] _ = False
noDub (a:as) xs 
   | (nTimesFound a xs) <= 1 && length(as) > 0 = noDub as xs
   | (nTimesFound a xs) <= 1 && length(as) == 0 = True
   | length(as) == 0 = True
   | otherwise = False

nTimesFound :: Eq a => a -> [a] -> Integer
nTimesFound _ [] = 0
nTimesFound x list = sum $ map (\a -> 1) $ filter (== x) list

nTimesFound2 :: Eq a => a -> [a] -> Nat0
nTimesFound2 _ [] = 0
nTimesFound2 x list = sum $ map (\a -> 1) $ filter (== x) list

-- delete dublicates from list1 found in list2
-- [1,2,3] [4,2,6] -> [1,3]
delDubl :: Eq a => [a] -> [a] -> [a]
delDubl (a:as) bs 
   | not(any (==a) bs) && length(as) > 0   = [a]  ++ delDubl as bs
   | not(any (==a) bs) && length(as) == 0 = [a]
   | (any (==a) bs) && length(as) > 0  = [] ++ delDubl as bs
   | (any (==a) bs) && length(as) == 0  = []


------ schneide
onlyDub :: Eq a => [a]-> [a] -> [a]
onlyDub [] _ = []
onlyDub as [] = as
onlyDub (a:as) bs = if (nTimesFound a bs) == 1 then [a] ++ onlyDub as bs else [] ++ onlyDub as bs 





{- Knapp, aber gut nachvollziehbar gehen die drei Instanzbildungen fuer 
   Menge_von folgendermassen vor:
   ...
-}




-- Aufgabe A.4

instance (Eq a,Eq b) => Menge_von (Paar a b) where
   vereinige as bs
      | (noDub as as) && (noDub bs bs) = union as bs
      | otherwise = error "Fehler"
   schneide as bs
      | (noDub as as) && (noDub bs bs) = onlyDub as bs
      | otherwise = error "Fehler"
   ziehe_ab as bs
      | (noDub as as) && (noDub bs bs) = [x | x <- as, not(elem x bs)]
      | otherwise = error "Fehler"   

   ist_teilmenge as bs
      | (noDub as as) && (noDub bs bs) = length(onlyDub as bs) == length(as)
      | otherwise = error "Fehler" 
   ist_leer as
      | (noDub as as) = length(as) == 0
      | otherwise = error "Fehler" 
   anzahl x as 
      | (noDub as as) = (nTimesFound2 (x) as) 
      | otherwise = error "Fehler" 



instance Eq a => Menge_von (Baum a) where
   vereinige as bs 
      | (noDub as as) && (noDub bs bs) = union as bs
      | otherwise = error "Fehler"
   schneide as bs
      | (noDub as as) && (noDub bs bs) = onlyDub as bs
      | otherwise = error "Fehler"
   ziehe_ab as bs
      | (noDub as as) && (noDub bs bs) = [x | x <- as, not(elem x bs)]
      | otherwise = error "Fehler"   

   ist_teilmenge as bs
      | (noDub as as) && (noDub bs bs) = length(onlyDub as bs) == length(as)
      | otherwise = error "Fehler" 
   ist_leer as
      | (noDub as as) = length(as) == 0
      | otherwise = error "Fehler" 
   anzahl x as 
      | (noDub as as) = (nTimesFound2 x as) 
      | otherwise = error "Fehler" 
   

ttl :: Baum b -> [b]
ttl (Blatt a) = [a]
ttl (Knoten ltb a rtb) =  ttl ltb ++ [a] ++ ttl rtb



{- Knapp, aber gut nachvollziehbar gehen die beiden Instanzbildungen fuer 
   Menge_von folgendermassen vor:
   ...
-}



-- Aufgabe A.5

instance Eq a => Eq (ElemTyp a) where 
   (==) (ET a) (ET b) = (a == b)

instance Show a => Show (ElemTyp a) where 
   show (ET a) = show a

{- Knapp, aber gut nachvollziehbar gehen die beiden Instanzbildungen fuer 
   Eq und Show folgendermassen vor:
   ...
-}



-- Aufgabe A.6

instance Eq a => Menge_von (ElemTyp a) where 
   vereinige a b = a ++ b
   schneide as bs = (durchschnitt as bs)--[x | x <- a, (elem x b)] 
   ziehe_ab as bs = as \\ bs -- (durchschnitt as bs)
   ist_teilmenge as bs = (length as <= length bs) && (schneide as bs == as)
   ist_leer as = length(as) == 0
   anzahl x as = (nTimesFound2 (x) as) 


durchschnitt :: Eq a => [a] -> [a] -> [a]
durchschnitt [] _  = []
durchschnitt _ []  = []
durchschnitt as bs = (as \\ diff)
  where diff = as \\ bs



{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer 
   Menge_von folgendermassen vor:
   ...
-}




-- Aufgabe A.7

instance (Eq a,Eq b,Eq c,Eq d,Eq e) => Menge_von (PH_ElemTyp a b c d e) where 
   vereinige as bs
      | (noDub as as) && (noDub bs bs) = union as bs
      | otherwise = error "Fehler"
   schneide as bs
      | (noDub as as) && (noDub bs bs) = onlyDub as bs
      | otherwise = error "Fehler"
   ziehe_ab as bs
      | (noDub as as) && (noDub bs bs) = [x | x <- as, not(elem x bs)]
      | otherwise = error "Fehler"   

   ist_teilmenge as bs
      | (noDub as as) && (noDub bs bs) = length(onlyDub as bs) == length(as)
      | otherwise = error "Fehler" 
   ist_leer as
      | (noDub as as) = length(as) == 0
      | otherwise = error "Fehler" 
   anzahl x as 
      | (noDub as as) = (nTimesFound2 (x) as) 
      | otherwise = error "Fehler" 

{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer 
   Menge_von folgendermassen vor:
   ...
-}




-- Aufgabe A.8

instance (Eq p,Eq q,Eq r) => Menge_von (PH_ElemTyp' p q r) where
   vereinige a b = a ++ b
   schneide as bs =  (durchschnitt as bs)-- [x | x <- a, (elem x b)] 
   ziehe_ab as bs = as \\ bs
   ist_teilmenge as bs = (length as <= length bs) && (schneide as bs == as)
   ist_leer as = length(as) == 0
   anzahl x as = (nTimesFound2 (x) as) 



{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer 
   Menge_von folgendermassen vor:
   ...
-}

