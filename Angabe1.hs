module Angabe1 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}


type Nat0                = Int
type Zeichenreihe        = String
type Teilzeichenreihe    = String
type IstTeilzeichenreihe = Bool
type Zerlegungszeuge     = (Zeichenreihe,Zeichenreihe,Zeichenreihe)
type Zerlegungszeugen    = [Zerlegungszeuge]

slice off n = (take n).(drop off)


-- Aufgabe A.1

{- Knapp, aber gut nachvollziehbar geht ist_tzr folgendermassen vor:
   Falls der input kürzer ist als das Muster = false. 
   Fundierung = falls input gleich muster. 
   Fortschritt = falls input größer als muster und das herausgeschnittene Wort mit gleicher Länge nicht gleich, dann erser Buchstabe abschneiden.
   where: slice schneidet aus dem input-String beginnend am Anfang ein Wort mit Länge des muster-Worts heraus.
-}
ist_tzr :: Zeichenreihe -> Teilzeichenreihe -> IstTeilzeichenreihe
ist_tzr input muster 
   | (length input) < (length muster) = False
   | (input == muster) = True
   | ((length input) > (length muster) && not((slice 0 len input) == muster)) = ist_tzr (tail input) muster 
   | ((slice 0 len input) == muster) = True
   | otherwise = False
   where slice 0 len = (take len).(drop 0)
         len = length muster
     




-- Aufgabe A.2

{- Knapp, aber gut nachvollziehbar geht tzr_zeuge folgendermassen vor: 
   Falls input kleiner als muster oder das muster generell nicht im input enthalten, gesondeter Triplefall.
   Hier wird das Ergebnis direkt mit Hilfe ausgelagerter Hilfsfunktion und Rekursion erstellt.
   Die Hilfsfunktion "help_tzr_zeuge" ermittelt den (Begin-)Index des Musterworts im Inputwort. 
-}
tzr_zeuge :: Zeichenreihe -> Teilzeichenreihe -> Zerlegungszeuge
tzr_zeuge input muster 
   | ((length input) < (length muster) || not(ist_tzr input muster)) = ("", muster++muster,"")
   | otherwise = ((first_half),    muster     , (second_half))
   where begin = help_tzr_zeuge 0 input muster
         first_half = take begin input
         end = length input
         end_muster = begin+(length muster)
         second_half = drop end_muster input



{- Ziel: Funktion ermittelt den (Begin-)Index des Musterworts im Inputwort-}
help_tzr_zeuge :: Nat0 -> Zeichenreihe -> Teilzeichenreihe -> Nat0
help_tzr_zeuge counter input muster 
   | (length input) < (length muster) = 0
   | (input == muster) = counter
   | ((length input) > (length muster) && not((slice 0 len input) == muster)) = help_tzr_zeuge (counter + 1) (tail input) muster 
   | ((slice 0 len input) == muster) = counter
   | otherwise = 0
   where slice 0 len = (take len).(drop 0)
         len = length muster



-- Aufgabe A.3

{- Knapp, aber gut nachvollziehbar geht tzr_zeugen folgendermassen vor: 
   1. Indizes mittels indexErmittler Fkt ermitteln -> bspw. "helloxxhello" mit muster "hello" = [0,7]
      1b.indexErmittler gibt eine Liste mit tatsächlichen Indizes und Platzhalter als "-1" aus, z.B. [0,-1,-1,-1,-1,-1,-1,7],  minusEinsLöscher Fkt löscht -1 aus dieser Liste heraus
   2. tzr_zeugen ruft Hilfs-Fkt. mit Indexliste auf -> help_tzr_zeugen (Sinn: eigene(=nützlicherere) Signatur) erstellt Zerlegungszeugen (Liste mit Tupeln), help_tzr_zeugen_leeres_muster-Fkt
      hilft, wenn das muster == "" ist (rekursiver Aufruf mit counter)

   Struktur:
   tzr_zeugen()
      | help_tzr_zeugen()
         | help_tzr_zeugen_leeres_muster()
      | indexErmittler()
         | minusEinsLöscher()
-}

tzr_zeugen :: Zeichenreihe -> Teilzeichenreihe -> Zerlegungszeugen
tzr_zeugen input muster = 
   help_tzr_zeugen input muster (indexErmittler input muster)

help_tzr_zeugen :: Zeichenreihe -> Teilzeichenreihe -> [Int] -> Zerlegungszeugen
help_tzr_zeugen input muster list
 | input == "" = []
 | muster == "" = help_tzr_zeugen_leeres_muster 0 input "" -- wenn leere Liste
 | list == [] = []
 | otherwise = [(start, muster, ende)] ++ help_tzr_zeugen input muster (tail list)
 where start = take (head list) input -- nimmt ersten Buchstaben
       ende = drop (head list + (length muster)) input  -- nimmt letzten Buchstaben

help_tzr_zeugen_leeres_muster :: Nat0 -> Zeichenreihe -> Zeichenreihe -> Zerlegungszeugen
help_tzr_zeugen_leeres_muster counter input muster 
 | (counter >= (length input)) = [(start, muster, ende)]
 | otherwise = [(start, muster, ende)] ++ help_tzr_zeugen_leeres_muster (counter + 1) input muster
 where start = take counter input
       ende = drop counter input

indexErmittler :: String -> String -> [Int]
indexErmittler input muster
   | (input == "") = []
   | (muster == "") = [if True then 1 else -1 | x <- [0..(length input -1)]]
   | otherwise = minusEinsLöscher [if input!!x == (head muster) then (if ist_tzr (slice x (length muster) input) muster then x else -1) else -1 | x <- [0..(length input -1)]]


minusEinsLöscher :: [Int] -> [Nat0]
minusEinsLöscher [] = []
minusEinsLöscher list = [list!!x | x <- [0..(length list -1)], list!!x /= -1 ]


 


-- Aufgabe A.4

{- Knapp, aber gut nachvollziehbar geht wieOft folgendermassen vor: 
   ... 
-}
wieOft :: Zeichenreihe -> Teilzeichenreihe -> Nat0
wieOft input muster
   | (length input < length muster) = 0
   | (muster == "") = length (indexErmittler input muster) + 1 
   | otherwise = length (indexErmittler input muster)

