module Angabe2 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}


-- Aufgabe A.1

-- Ergaenzen Sie fehlende Typklassen in deriving-Klauseln, wo noetig und nicht explizit
-- eine Instanz-Deklaration gefordert ist.


type Nat1              = Int
newtype Vorname        = Vorname String deriving (Eq,Ord,Show)
newtype Nachname       = Nachname String deriving (Eq,Ord,Show)
data VHDS              = Viertel | Halb | Dreiviertel | Schlag deriving (Eq,Ord, Show)
data Stunde            = Eins | Zwei | Drei | Vier | Fuenf | Sechs
                         | Sieben | Acht | Neun | Zehn | Elf 
                         | Zwoelf deriving (Eq,Ord,Enum, Show)
data VorNachMittag     = VM | NM deriving (Eq,Ord, Show)
newtype Uhrzeit        = U (VHDS,Stunde,VorNachMittag) deriving (Eq, Ord)
data Tag               = I | II | III | IV | V | VI | VII | VIII | IX | X
                         | XI | XII | XIII | XIV | XV | XVI | XVII | XVIII 
                         | XIX | XX | XXI | XXII | XXIII | XXIV | XXV
                         | XXVI | XXVII | XXVIII | XXIX | XXX 
                         | XXXI deriving (Eq,Ord,Enum, Show)
data Monat             = Jan | Feb | Mar | Apr | Mai | Jun 
                         | Jul | Aug | Sep | Okt | Nov | Dez deriving (Eq,Ord,Enum, Show)
type Jahr              = Nat1
data Datum             = D Tag Monat Jahr deriving (Eq, Ord)
data Testart           = PCR | Antigen deriving (Eq, Ord, Show)
data Impfstoff         = AstraZeneca | BioNTec | JundJ | Moderna 
                         | Sputnik | Sinovac deriving (Eq,Ord,Show)
data Anzahl            = Einmal | Zweimal deriving (Eq, Ord, Show)
data DreiG_Status      = Geimpft (Impfstoff,Anzahl) | Genesen 
                         | Getestet Testart Datum Uhrzeit 
                         | Udrei deriving (Eq,Ord,Show)
                           -- Udrei: Ungetestet, Ungenesen, Ungeimpft
data Regel             = DreiG | ZweieinhalbG | ZweiG deriving Eq
data Person            = P Vorname Nachname DreiG_Status deriving (Eq,Ord,Show)
type Einlassbegehrende = [Person]
type VorUndNachname    = String
type Einzulassende     = [VorUndNachname]
type Abzuweisende      = [VorUndNachname]
type Kontrollzeitpunkt = (Datum,Uhrzeit)
data Kontrollergebnis  = Einlassen | Abweisen | Ungueltig deriving (Eq,Show)

newtype Iso_uhr        = IsoU (VorNachMittag, Stunde, VHDS) deriving (Eq, Ord, Show)
data Iso_datum         = IsoD Jahr Monat Tag deriving (Eq, Ord, Show)



-- Aufgabe A.2
{- Knapp, aber gut nachvollziehbar geht einzulassen folgendermassen vor:
 | -- Kontrollzeitpunkt --> wenn falsch --> Ungueltig --
 | -- Udrei ==  Abweisen
 | -- genesen == Einlassen
 | -- geimpft (hier Methode(status)) --> wenn ok, dann Einlassen
 | -- Regel == ZweiG --> dann Abweisen
 | -- test-PCR (hier Methode(status)) --> wenn ok, dann Einlassen 
 | -- Regel == ZweieinhalbG --> dann Abweisen
 | -- test-Antigen (hier Methode(status)) --> wenn ok, dann Einlassen 
 | -- otherwise --> Abweisen
-}
einzulassen :: (Person,Regel,Kontrollzeitpunkt) -> Kontrollergebnis
einzulassen (person,regel,kzp)
 | (kzpCHECK kzp) == False = Ungueltig
 | getPStatus person == Udrei = Abweisen
 | getPStatus person == Genesen = Einlassen
 | (geimpftePerson(getPStatus person) == True && geimpftCHECK (getPStatus person) == True) = Einlassen 
 | regel == ZweiG = Abweisen
 | getestetPerson(getPStatus person) == True && getTestart (getPStatus person) == PCR && pcrCHECK (getPStatus person) kzp == True  = Einlassen
 | regel == ZweieinhalbG = Abweisen
 | getestetPerson(getPStatus person) == True && getTestart (getPStatus person) == Antigen && antigenCHECK (getPStatus person) kzp == True  = Einlassen
 | otherwise = Abweisen
   where getPVorname (P a _ _) = a
         getPNachname (P _ a _) = a
         getPStatus (P _ _ a) = a
         getKDatum (a, _) = a
         getKUhrzeit (_, a) = a



-- EIGENE BEISPIELE
{-
ührli :: [Stunde]
ührli = [Eins, Zwei, Drei]
day = D III Mar 2016 :: Datum
uhr = U (Halb, Neun, VM)
day2 = D II Mar 2016 :: Datum
uhr2 = U (Halb, Acht, VM)
kzp = (day, uhr)

pcr = Getestet PCR day uhr :: DreiG_Status
dgs1 = Getestet PCR (D XX Okt 2021) (U (Viertel,Acht,VM)) :: DreiG_Status
bm = P (Vorname "Michael") (Nachname "Ludwig") dgs1 :: Person
-}

-- BEISPIELE



{- ALLES KORREKT
einzulassen (bgm,DreiG,kzp1) ->> Einlassen
einzulassen (bgm,DreiG,kzp2) ->> Abweisen 
einzulassen (bgm,DreiG,kzp3) ->> Ungueltig
einzulassen (bm,DreiG,kzp1) ->> Abweisen 
einzulassen (bm,DreiG,kzp2) ->> Abweisen 
einzulassen (bm,DreiG,kzp3) ->> Ungueltig

einzulassen (bk,DreiG,kzp1) ->> Einlassen
einzulassen (bk,DreiG,kzp2) ->> Einlassen
einzulassen (bk,DreiG,kzp3) ->> Ungueltig
einzulassen (bp,ZweieinhalbG,kzp1) ->> Abweisen
einzulassen (bp,ZweieinhalbG,kzp2) ->> Abweisen
einzulassen (bp,ZweieinhalbG,kzp3) ->> Ungueltig
-}


-- hier statt Typabfrage "Geimpft" ganze Methode
geimpftePerson :: DreiG_Status -> Bool
geimpftePerson (Geimpft (_,_)) = True
geimpftePerson x = False

getestetPerson :: DreiG_Status -> Bool
getestetPerson (Getestet _ _ _) = True
getestetPerson x = False




getTestart :: DreiG_Status -> Testart
getTestart status = gettestart status
   where gettestart (Getestet a _ _) = a 

getTestDatum :: DreiG_Status -> Datum
getTestDatum status = getTestdatum status
   where getTestdatum (Getestet _ a _) = a 

getTestUhrzeit :: DreiG_Status -> Uhrzeit
getTestUhrzeit status = getTestuhrzeit status
   where getTestuhrzeit (Getestet _ _ a) = a 





-- (Impfstoff,Anzahl)
geimpftCHECK :: DreiG_Status -> Bool
geimpftCHECK impfstoff  
 | getImpftstoff impfstoff ==  Sputnik = False
 | getImpftstoff impfstoff ==  Sinovac = False 
 | getAnzahl impfstoff == Einmal = False
 | otherwise = True
   where getImpftstoff (Geimpft (a, _)) = a 
         getAnzahl (Geimpft (_, a)) = a  


kzpCHECK :: Kontrollzeitpunkt -> Bool
kzpCHECK kzp = datumsCHECK (getKDatum kzp)
  where getKDatum (a, _) = a
        getKUhrzeit (_, a) = a


datumsCHECK :: Datum -> Bool
datumsCHECK date
 | (schaltjahrCHECK (getJahr date) == False && getTag date >= XXIX && getMonat date == Feb) = False
 | (getTag date == XXXI) && ((getMonat date == Apr ) || (getMonat date == Jun) ||(getMonat date == Sep) ||(getMonat date == Nov)) = False
 | getJahr date > 2021 = False
 | otherwise = True
   where getTag (D a _ _) = a
         getMonat (D _ a _) = a
         getJahr (D _ _ a) = a

schaltjahrCHECK :: Jahr -> Bool
schaltjahrCHECK jahr
 | mod jahr 4 == 0 && mod jahr 400 == 0 = True
 | mod jahr 4 == 0 && mod jahr 100 == 0 && mod jahr 400 /= 0 = False
 | mod jahr 4 == 0 = True
 | otherwise = False


convertUhrzeit :: Uhrzeit -> Iso_uhr
convertUhrzeit (U (a,b,c)) = (IsoU (c,b,a))

convertDatum :: Datum -> Iso_datum
convertDatum (D a b c) = (IsoD c b a)



pcrCHECK :: DreiG_Status -> Kontrollzeitpunkt -> Bool
pcrCHECK pcr_test kzp = test72Gueltig kzp (getTestDatum pcr_test) (getTestUhrzeit pcr_test)

antigenCHECK :: DreiG_Status -> Kontrollzeitpunkt -> Bool
antigenCHECK antigen_test kzp = test24Gueltig kzp (getTestDatum antigen_test) (getTestUhrzeit antigen_test)


-- Antigentest
-- kleiner bedeutet älter
test24Gueltig :: Kontrollzeitpunkt -> Datum -> Uhrzeit -> Bool
test24Gueltig kzp tdate tuhr 
 | getKDatum kzp == tdate = True
 | convertDatum datum24 < convertDatum tdate = True
 | convertDatum datum24 <= convertDatum tdate && convertUhrzeit (getKUhrzeit kzp) <= convertUhrzeit tuhr  = True
 | otherwise = False
  where datum24 = davor24 (getKDatum kzp)
        getKDatum (a, _) = a
        getKUhrzeit (_, a) = a

-- PCR-Test
test72Gueltig :: Kontrollzeitpunkt -> Datum -> Uhrzeit -> Bool
test72Gueltig kzp tdate tuhr 
 | getKDatum kzp == tdate = True
 | convertDatum datum72 < convertDatum tdate = True
 | convertDatum datum72 <= convertDatum tdate && convertUhrzeit (getKUhrzeit kzp) <= convertUhrzeit tuhr  = True
 | otherwise = False
  where datum72 = davor72 (getKDatum kzp)
        getKDatum (a, _) = a
        getKUhrzeit (_, a) = a



davor24 :: Datum -> Datum 
davor24 date 
 | (getDtag date == I && getDmon date == Mar && schaltjahrCHECK (getDjahr date) == False) = (D ((XXVIII)) (Feb) (getDjahr date))
 | (getDtag date == I && getDmon date == Mar && schaltjahrCHECK (getDjahr date) == True) = (D ((XXIX)) (Feb) (getDjahr date))
 | getDmon date == Jan && getDtag date == I  = (D ((XXXI)) (Dez) ((getDjahr date)-1)) -- Jahreswende
 | (getDtag date == I && (getDmon date == Mai || getDmon date == Jul || getDmon date  == Okt ||getDmon date == Dez)) = (D ((XXX)) (pred (getDmon date)) (getDjahr date)) -- Vormonate mit 30 Tagen
 | ((getDtag date == I ) && not(getDmon date == Mai || getDmon date == Jul || getDmon date  == Okt ||getDmon date == Dez)) = (D ((XXXI)) (pred (getDmon date)) (getDjahr date)) -- Vormonate mit 31 Tagen
 | getDtag date /= I = (D (pred (getDtag date)) (getDmon date) (getDjahr date)) -- normale Rückgang -> Tag
 where getDtag (D a _ _) = a
       getDmon (D _ a _) = a
       getDjahr (D _ _ a) = a



davor72 :: Datum -> Datum
davor72 date = davor24(davor24(davor24 date))



-- Aufgabe A.3
{- Knapp, aber gut nachvollziehbar geht einzulassende folgendermassen vor: 
    einzulassen :: (Person,Regel,Kontrollzeitpunkt) -> Kontrollergebnis 
-}
einzulassende :: Einlassbegehrende -> Regel -> Kontrollzeitpunkt -> Einzulassende
einzulassende personen regel kzp = [nameExtraktor x | x <- personen, tuerSteher x regel kzp == Einlassen]

tuerSteher :: Person -> Regel -> Kontrollzeitpunkt -> Kontrollergebnis 
tuerSteher person regel kzp
 | einzulassen (person, regel, kzp) == Einlassen = Einlassen
 | einzulassen (person, regel, kzp) == Abweisen = Abweisen
 | otherwise = Ungueltig

nameExtraktor :: Person -> String 
nameExtraktor (P (Vorname vorname) (Nachname nachname) _) = vorname ++ " " ++ nachname


-- Aufgabe A.4
{- Knapp, aber gut nachvollziehbar geht einzulassende_abzuweisende folgendermassen vor: 
   ... 
-}
einzulassende_abzuweisende :: Einlassbegehrende -> Regel -> Kontrollzeitpunkt -> (Einzulassende,Abzuweisende)
einzulassende_abzuweisende personen regel kzp = ([nameExtraktor x | x <- personen, tuerSteher x regel kzp == Einlassen],[nameExtraktor x | x <- personen, tuerSteher x regel kzp == Abweisen])


{-
show (U (Viertel,Zwoelf,VM)) ->> "11:15 Uhr"
show (U (Viertel,Zwoelf,NM)) ->> "23:15 Uhr"
show (U (Dreiviertel,Zwoelf,VM)) ->> "11:45 Uhr"

show (U (Dreiviertel,Zwoelf,NM)) ->> "23:45 Uhr"
show (U (Schlag,Zwoelf,VM)) ->> "12:00 Uhr"
show (U (Schlag,Zwoelf,NM)) ->> "24:00 Uhr"
show (U (Halb,Sechs,VM)) ->> "05:30 Uhr"
show (U (Halb,Sechs,NM)) ->> "17:30 Uhr"
show (D XXII Okt 2021) ->> "22.10.2021"
show (D XXIV Dez 2412) ->> "24.12.2412"
show (D I Jan 1) ->> "1.1.1"
show (D V Feb 54321) ->> "5.2.54321"
show (D XXXI Feb 1234) ->> "Datum ungueltig"
-}

-- Aufgabe A.5
{- Knapp, aber gut nachvollziehbar geht die Implementierung von show fuer Uhrzeit 
   folgendermassen vor:
   ...
-}
instance Show Uhrzeit where
 show (U (vhds,stunde,vnm)) 
   | vnm == VM && vhds == Schlag = hour_10_schlag ++ ":" ++ vhdsToString vhds ++ " Uhr"
   | vnm == VM && vhds /= Schlag = hour_10 ++ ":" ++ vhdsToString vhds ++ " Uhr"
   | vnm == NM && vhds == Schlag = show((findIstud stunde)+12) ++ ":" ++ vhdsToString vhds ++ " Uhr"
   | vnm == NM && vhds /= Schlag = show((findIstud stunde)+11) ++ ":" ++ vhdsToString vhds ++ " Uhr"
   where hour = (findIstud stunde) 
         hour_10_schlag = if(hour < 10) then "0"++ show(hour) else show(hour)  
         hour_10 = if((hour-1) < 10) then "0"++ show((hour-1)) else show((hour-1))  

vhdsToString :: VHDS -> String 
vhdsToString Viertel = "15"
vhdsToString Halb = "30"
vhdsToString Dreiviertel = "45"
vhdsToString Schlag = "00"


{- Knapp, aber gut nachvollziehbar geht die Implementierung von show fuer Datum 
   folgendermassen vor:
   ...
-}
instance Show Datum where  
 show (D tag monat jahr) 
   | dateCHECK (D tag monat jahr) == False = "Datum ungueltig"
   | otherwise = day ++ "." ++ month ++ "." ++ show(jahr)
   where day = show(findItag tag)
         month = show(findImon monat)

findIstud :: Stunde -> Int 
findIstud a = (head $ filter ((== a) . ((enumFrom Eins) !!)) [0..])+1

 

findItag :: Tag -> Int 
findItag a = (head $ filter ((== a) . ((enumFrom I) !!)) [0..])+1

findImon :: Monat -> Int 
findImon a = (head $ filter ((== a) . ((enumFrom Jan) !!)) [0..])+1

dateCHECK :: Datum -> Bool
dateCHECK date
 | (schaltjahrCHECK (getJahr date) == False && getTag date >= XXIX && getMonat date == Feb) = False
 | (getTag date == XXXI) && ((getMonat date == Apr ) || (getMonat date == Jun) ||(getMonat date == Sep) ||(getMonat date == Nov)) = False
 | otherwise = True
   where getTag (D a _ _) = a
         getMonat (D _ a _) = a
         getJahr (D _ _ a) = a


datekl = (D XXIX Feb 2000) :: Datum








{- Knapp, aber gut nachvollziehbar gehen die Implementierungen von show folgendermassen vor: 
   ... 
-}
-- Aufgabe A.5

--instance Show Uhrzeit where
 --show ...


--instance Show Datum where
 --show ...






