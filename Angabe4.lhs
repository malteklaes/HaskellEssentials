> module Angabe4 where

1. Vervollstaendigen Sie gemaess Angabentext!
2. Vervollst�ndigen Sie auch die vorgegebenen Kommentaranf�nge!
3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat! 


Datenstrukturen fuer eine einfache Buchhaltung:

> type Nat1    = Int
> type Name    = String
> newtype Cent = C { cents :: Nat1
>                  } deriving (Eq,Ord,Show)
> type Brutto  = Cent
> type Netto   = Cent
> data Skonto  = KeinSkonto | DreiProzent  
>                | FuenfProzent | ZehnProzent deriving (Eq,Ord,Show)
> data Tag     = I | II | III | IV | V | VI | VII | VIII | IX | X
>                | XI | XII | XIII | XIV | XV | XVI | XVII | XVIII 
>                | XIX | XX | XXI | XXII | XXIII | XXIV | XXV
>                | XXVI | XXVII | XXVIII | XXIX | XXX 
>                | XXXI deriving (Eq,Ord,Show)
> data Monat   = Jan | Feb | Mar | Apr | Mai | Jun | Jul | Aug
>                | Sep | Okt | Nov | Dez deriving (Eq,Ord,Show, Enum)
> type Jahr    = Nat1
> data Datum   = D { tag   :: Tag,
>                    monat :: Monat,
>                    jahr  :: Jahr 
>                  } deriving (Eq, Show)

> data Geschaeftspartner = GP { partner :: Name,
>                               seit :: Datum
>                              } deriving (Eq,Show)
> data Geschaeftsvorfall = Zahlung { brutto :: Brutto,
>                                    skonto :: Skonto,
>                                    zahlung_vom :: Datum
>                                  }
>                          | Gutschrift { gutschriftsbetrag :: Cent,
>                                         gutschrift_vom :: Datum
>                                       } 
>                          deriving (Eq,Show)
> type Kassabucheintrag = (Geschaeftspartner,Geschaeftsvorfall) 
> newtype Kassabuch = KB [Kassabucheintrag] deriving (Eq,Show)

-------------------------------------------------------------------------------- Instances

> instance Num Cent where
>   (+) (C c1) (C c2) = (C (c1 + c2))
>   (-) (C c1) (C c2) = (C (c1 - c2))
>   (*) (C c1) (C c2) = (C (c1 * c2))
>   abs (C c1)        = (C (absolute c1))

> instance Num EuroCent where
>   (+) (EC e1 c1) (EC e2 c2) = centToEurocent(C (e1*100 + e2*100 + c1 + c2))
>   (-) (EC e1 c1) (EC e2 c2) = diffEC (EC e1 c1) (EC e2 c2) 
>   negate  (EC e1 c1)             = (EC (-e1) c1)
>   fromInteger n = EC (fromInteger n) 0

> diffEC :: EuroCent -> EuroCent -> EuroCent
> diffEC (EC e1 c1) (EC e2 c2) 
>   |   (e1 > e2)  = centToEurocent(C ( ((e1*100+c1) - (e2*100+c2))))
>   |   (e1 < e2)  = negate(centToEurocent(C (((e2*100+c2) - (e1*100+c1)))))
>   |   otherwise = (EC 0 0)


> instance Ord Datum where
>    compare d1 d2
>     |  jahr d1 == (jahr d2) && (monat d1) == (monat d2) = compare (tag d1) (tag d2)
>     |  jahr d1 == (jahr d2) = compare (monat d1) (monat d2)
>     |  otherwise = compare (jahr d1) (jahr d2)

> instance Ord Geschaeftspartner where
>    compare p1 p2
>     |  partner p1 == (partner p2) = compare (seit p1) (seit p2)
>     |  otherwise = compare (partner p1) (partner p2)

--------------------------------------------------------------------------------


Aufgabe A.1 --------------------------------------------------------------------------------

> type P_Geschaeftspartner = Geschaeftspartner
> data AP_Geschaeftsvorfall 
>    = AP_Zahlung { netto :: Netto,
>                   zahlungsdatum :: Datum
>                 }
>     | P_Gutschrift { gutschrift :: Cent,
>                      gutschriftsdatum :: Datum
>                    } deriving (Eq,Show)   
> type AP_Kassabucheintrag = (P_Geschaeftspartner,AP_Geschaeftsvorfall)

> waup :: Kassabucheintrag -> AP_Kassabucheintrag
> waup ( (GP name datum1) , (Zahlung brutto skonto datum2)) = ((GP name (datumsCorrect datum1)) ,  (AP_Zahlung (bruttoToNetto brutto skonto) (datumsCorrect datum2) ))
> waup ( (GP name datum1) , (Gutschrift cent datum2)) = ((GP name (datumsCorrect datum1)) ,  (P_Gutschrift cent (datumsCorrect datum2) ))


Knapp, aber gut nachvollziehbar geht waup folgendermassen vor: 
...


-------------------Variablen

 schaltdate = (D IX Feb 2000)
 normaldate = (D I Feb 2002)
 schaddate = (D XXXI Nov 1999)
 futuredate = (D XXXI Nov 2022)
 mon1 = Jan
 mon2 = Feb
 money1 = (C 12) -- 12 cents
 money2 = (C 2)
 money3 = (C (-2))  -- Frage ob überhaupt erlaubt --> siehe Vereinbarung
 nat1 = 12 :: Nat1
 prim = 7 :: Nat1
 frax = 2.3  
 num1 = 2 :: Int
 skont = DreiProzent :: Skonto
 brutt = 273 :: Brutto

-------------------Test-Cases A.1    (Muster: (Geschaeftspartner,Geschaeftsvorfall) )

 testa1_1 = (GP "Mr Monopoly" (D XXXI Nov 1969), Zahlung (C 273) DreiProzent (D XXXI Dez 2000))
 testa1_2 = (GP "Mr Monopoly" (D XXXI Nov 10520), Zahlung (C 15024) KeinSkonto (D XXXI Dez 50123))
 testa1_3 = (GP "Mr Monopoly" (D XXIX Feb 2000), Zahlung (C 0) FuenfProzent (D XXIX Feb 2001))

 kbein = (GP "Hans Jurgen" (D XXI Nov 1969), Zahlung (C 273) DreiProzent (D XXXI Dez 2000))
 kbein1 = (GP "Uwe" (D XXI Nov 1956), Zahlung (C 273) FuenfProzent (D XXXI Dez 2000))

--------------------------------------Hilfs-Funktionen
-------------------Funktionen-Plausibilität

> gpCheck :: P_Geschaeftspartner -> P_Geschaeftspartner -- ein Geschöftspartner wird nach seinem Datum überprüft
> gpCheck (GP n d) = (GP n (datumsCorrect d))

-------------------Funktionen-Netto

> convertInt = 0 :: Int
> bruttoToNetto :: Brutto -> Skonto -> Netto
> bruttoToNetto b s = (C  ((bruttoToNum1 b) - (floor((fromIntegral ((bruttoToNum1 b)*(skontoToNum1 s))*0.01)))    )) 

> btn :: Brutto -> Skonto -> Netto
> btn b s = (C 2)




> absolute :: Int -> Int
> absolute n 
>   |   (n < 0) = ((-1) * n)
>   |   otherwise = n   

-------------------Funktionen-Converter


> bruttoToNum1 :: Brutto -> Int
> bruttoToNum1 (C b) = b


> skontoToNum1 :: Skonto -> Int
> skontoToNum1 s 
>   |   s == DreiProzent = 3
>   |   s == FuenfProzent = 5
>   |   s == ZehnProzent = 10
>   |   s == KeinSkonto = 0
>   |   otherwise = 0

> nat1ToInt :: Nat1 -> Int
> nat1ToInt n = n

> getCents :: Cent -> Nat1 
> getCents (C c) = c

> centsConv :: Nat1 -> Cent 
> centsConv n = (C n)

-------------------Funktionen-Zeit

> datumsCorrect :: Datum -> Datum
> datumsCorrect (D t m y)
>   |   y > 2021 = (datumsCorrect (D t m 2021))
>   |   not(schaltjahrCHECK y) && (D t m y) == (D XXIX Feb y) = (D I Mar y)
>   |   ((D t m y) == (D XXX Feb y)) = (D I Mar y)
>   |   (D t m y) == (D XXXI m y) && ( (m == Feb) || (m == Apr) || (m == Jun) || (m == Sep) || (m == Nov)) = (D I (succ m) y)
>   |   otherwise = (D t m y)


> schaltjahrCHECK :: Jahr -> Bool
> schaltjahrCHECK jahr
>  | mod jahr 4 == 0 && mod jahr 400 == 0 = True
>  | mod jahr 4 == 0 && mod jahr 100 == 0 && mod jahr 400 /= 0 = False
>  | mod jahr 4 == 0 = True
>  | otherwise = False














Aufgabe A.2 --------------------------------------------------------------------------------

> data EuroCent = EC { euro :: Nat1,
>                      cent :: Nat1  

                       Nur Werte zwischen 0 und 99 fuer cent!

>                    } deriving (Eq,Ord,Show)

> data K_Geschaeftsvorfall = K_Zahlung { ec_netto :: EuroCent,
>                                        zahlungsdatum' :: Datum
>                                      }
>                            | K_Gutschrift { ec_gutschrift :: EuroCent,
>                                             gutschriftsdatum' :: Datum 
>                                           } deriving (Eq,Show)           

> newtype KonsolidiertesKassabuch
>     = KKB [(P_Geschaeftspartner,K_Geschaeftsvorfall)]
>       deriving (Eq,Show)   

> konsolidiere :: Kassabuch -> KonsolidiertesKassabuch
> konsolidiere (KB (xs)) =  KKB [   ( ( (getGP (x)) , (getGeschVorfall (x)) )  )  |    x <- xs ]


-------------------Test-Cases A.2    (Muster: (Geschaeftspartner,Geschaeftsvorfall) )

 kunde1 = (GP "Mr Monopoly" (D XXXI Nov 1969), Zahlung (C 273) DreiProzent (D XXXI Dez 2000))
 kunde2 = (GP "Mr Monopoly" (D XXXI Nov 10520), Zahlung (C 15024) KeinSkonto (D XXXI Dez 50123))
 kunde3 = (GP "Mr Monopoly" (D XXIX Feb 2000), Zahlung (C 0) DreiProzent (D XXIX Feb 2001))

 testa2_1 = (KB [kunde1, kunde2])
 testa2_2 = (GP "Mr Monopoly" (D XXXI Nov 10520), Zahlung (C 15024) FuenfProzent (D XXXI Dez 50123))
 testa2_3 = (GP "Mr Monopoly" (D XXIX Feb 2000), Zahlung (C 0) DreiProzent (D XXIX Feb 2001))



--------------------------------------Hilfs-Funktionen
-------------------Funktionen-Converter

> getGP :: Kassabucheintrag -> P_Geschaeftspartner
> getGP ((gp, gvorfall)) = plausiGP(gp)

> getGeschVorfall :: Kassabucheintrag -> K_Geschaeftsvorfall
> getGeschVorfall ((gp, gvorfall)) = (kons gvorfall)


-------------------hier wird hardcore konsolidiert

> plausiGP :: Geschaeftspartner -> P_Geschaeftspartner
> plausiGP (GP name datum) =  (GP name (datumsCorrect datum))

> kons :: Geschaeftsvorfall -> K_Geschaeftsvorfall 
> kons (Zahlung (C brutto)  (skonto)  (datum)) = (K_Zahlung (centToEurocent(bruttoToNetto (C brutto) skonto)) (datumsCorrect datum))
> kons (Gutschrift (C gutschriftsbetrag) gutschrift_vom) = (K_Gutschrift (centToEurocent (C gutschriftsbetrag)) (datumsCorrect gutschrift_vom))

> centToEurocent :: Cent -> EuroCent 
> centToEurocent (C n) 
>   |   n < 100 = (EC 0 n)
>   |   n >= 100 = (EC h e)
>   |   otherwise = (EC 0 0)
>   where e =  n - (h * 100)
>         h = (floor((fromIntegral (n)*0.01)))




Knapp, aber gut nachvollziehbar geht konsolidiere folgendermassen vor:
 ...










Aufgabe A.3 --------------------------------------------------------------------------------

> data Saldo = Forderungssaldo { fs :: EuroCent }
>              | Zahlungssaldo { zs :: EuroCent }
>              | Ausgeglichen
>              | Keine_Geschaeftsbeziehung deriving (Eq,Show)
                  
> saldo :: P_Geschaeftspartner -> KonsolidiertesKassabuch -> Saldo
> saldo gp (KKB (xs)) 
>    |  not(checkBoolList(findeGP(KKB (xs)) gp )) = Keine_Geschaeftsbeziehung  -- alternativ: not(any (==gp) [fst x | x <- (xs)]) = Keine_Geschaeftsbeziehung
>    |   sZ > sG = Zahlungssaldo (calcGesamtSumme (sZ) (sG))   
>    |   sZ < sG = Zahlungssaldo (calcGesamtSumme (sG) (sZ))
>    |   sZ == sG = Ausgeglichen 
>    |   otherwise = Keine_Geschaeftsbeziehung
>      where sZ = sum(summeZ (KKB (xs)) gp)
>            sG = sum(summeG (KKB (xs)) gp)


-------------------Test-Cases A.3   (Muster: (Geschaeftspartner,Geschaeftsvorfall) )

 kunde10 = (GP "Mr Monopoly" (D XXXI Nov 1969), Zahlung (C 273) DreiProzent (D XXXI Dez 2000))
 kunde12 = (GP "Mr Monopoly" (D XXXI Nov 10520), Zahlung (C 15024) KeinSkonto (D XXXI Dez 50123))
 kunde13 = (GP "Mr Monopoly" (D XXIX Feb 2000), Zahlung (C 0) DreiProzent (D XXIX Feb 2001))

 testa3_1 = (KB [kunde10, kunde13])
 testa3_2 = (GP "Mr Monopoly" (D XXXI Nov 10520), Zahlung (C 15024) FuenfProzent (D XXXI Dez 50123))
 testa3_3 = (GP "Mr Monopoly" (D XXIX Feb 2000), Zahlung (C 0) DreiProzent (D XXIX Feb 2001))

 plauskund1 = plausiGP (GP "Mr Monopoly" (D XXIX Feb 2000))


 kunde = GP "Mr Monopoly" (D XX Feb 2000)
 kunde20 = (GP "Mr Monopoly" (D XX Feb 2000), Zahlung (C 273) DreiProzent (D XXXI Dez 2000))
 kunde22 = (GP "Mr Monopoly" (D XX Nov 10520), Zahlung (C 15024) KeinSkonto (D XXXI Dez 50123))
 kunde23 = (GP "Mr Monopoly" (D XX Feb 2000), Gutschrift (C 400) (D XXI Feb 2001))

 test3 = konsolidiere(KB [kunde20, kunde23])

--------------------- Variablen

> g_vorfall1 = (K_Zahlung (EC 5 25) (D I Jan 1996)) :: K_Geschaeftsvorfall
> g_vorfall2 = (K_Gutschrift (EC 6 21) (D II Apr 2000)) :: K_Geschaeftsvorfall

> euro1 = (EC 5 26)
> euro2 = (EC 20 77) 
> euro3 = (EC 20 77) 
> euroList = [euro1, euro2, euro3] :: [EuroCent]
> x = sum euroList


------------------------------------------ Hilfsfunktionen


> calcGesamtSumme :: EuroCent -> EuroCent -> EuroCent
> calcGesamtSumme sum1 sum2= (sum1 - sum2) 

> kkbIter :: KonsolidiertesKassabuch -> KonsolidiertesKassabuch
> kkbIter (KKB (xs)) = KKB [ ((findP_GP x), (findK_Gvorfall x))  |  x <- xs]



> kkbIterK_Gvorfall :: KonsolidiertesKassabuch -> [K_Geschaeftsvorfall]
> kkbIterK_Gvorfall (KKB (xs)) =  [ (findK_Gvorfall x)  |  x <- xs]

> kkbIterP_Gpartner :: KonsolidiertesKassabuch -> [P_Geschaeftspartner]
> kkbIterP_Gpartner (KKB (xs)) =  [ (findP_GP x)  |  x <- xs]

--------------------- 1. Ebene Hilfsfunktionen

> findK_Gvorfall :: (P_Geschaeftspartner, K_Geschaeftsvorfall) -> K_Geschaeftsvorfall
> findK_Gvorfall  ((gp, gvorfall)) = gvorfall

> findP_GP :: (P_Geschaeftspartner, K_Geschaeftsvorfall) -> P_Geschaeftspartner
> findP_GP ((gp, gvorfall)) = gp


--------------------- 2. Eben Hilfsfunktionen
hier werden (Summe Zahlung) und (Summe Gutschriften) verrechnet


> summeZahlung :: [K_Geschaeftsvorfall] -> EuroCent
> summeZahlung [] = (EC 0 0)
> summeZahlung (x:xs) = (getZahlung x) + (summeZahlung xs) 


------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> summeZ :: KonsolidiertesKassabuch -> P_Geschaeftspartner -> [EuroCent]
> summeZ (KKB []) _ = [EC 0 0]
> summeZ (KKB kb) gp =  [  if (fst x == gp) then (getZahlung(snd x)) else (0) |   x <- kb]

> summeG :: KonsolidiertesKassabuch -> P_Geschaeftspartner -> [EuroCent]
> summeG (KKB []) _ = [EC 0 0]
> summeG (KKB kb) gp =  [  if (fst x == gp) then (getGutschrift(snd x)) else (0) |   x <- kb]


> findeGP :: KonsolidiertesKassabuch -> P_Geschaeftspartner -> [Bool]
> findeGP (KKB []) _ = [False]
> findeGP (KKB kb) gp = [  if (fst x == gp) then (True) else (False) |   x <- kb]

> checkBoolList :: [Bool] -> Bool
> checkBoolList [] = True
> checkBoolList (x:xs) = x && checkBoolList xs

------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> summeGutschrift :: [K_Geschaeftsvorfall] -> EuroCent
> summeGutschrift [] = (EC 0 0)
> summeGutschrift (x:xs) = (getGutschrift x) + (summeGutschrift xs) 

> findNameAndDate :: [P_Geschaeftspartner] -> Name -> Datum -> Bool
> findNameAndDate [] _ _ = False
> findNameAndDate [x] name date = (compareNameAndDate x name date)
> findNameAndDate (x:xs) name date = (compareNameAndDate x name date) && (findNameAndDate xs name date)

> compareNameAndDate :: Geschaeftspartner -> Name -> Datum -> Bool
> compareNameAndDate (GP name datum) cname cdatum = (name == cname)  -- && (datum == cdatum))

> getMoney :: K_Geschaeftsvorfall -> EuroCent
> getMoney (K_Zahlung ecent datum) = ecent
> getMoney (K_Gutschrift ecent datum) = ecent


> getZahlung :: K_Geschaeftsvorfall -> EuroCent
> getZahlung (K_Zahlung ecent datum) = ecent
> getZahlung (K_Gutschrift ecent datum) = (EC 0 0)

> getGutschrift :: K_Geschaeftsvorfall -> EuroCent
> getGutschrift (K_Gutschrift ecent datum) = ecent
> getGutschrift (K_Zahlung ecent datum) = (EC 0 0)






Knapp, aber gut nachvollziehbar geht saldo folgendermassen vor: 
...








Aufgabe A.4 --------------------------------------------------------------------------------
(convertToKB(sortKB(convertKB kb)) dieser Part ordnet das Kassabuch alphabetisch
getSaldo iteriert dann über die Liste, um die Saldos für die einzelnen Partner zu errechnen

> newtype SaldiertesKassabuch = SKB [(Geschaeftspartner,Saldo)] deriving (Eq,Show)

> saldiere :: Kassabuch -> SaldiertesKassabuch
> saldiere (KB kb) = SKB [(x, saldo x (konsolidiere(KB kb))) | x <- (sortKB [fst x | x <- kb])] -- doppelte ListComprehension



> sortKB :: [Geschaeftspartner] -> [Geschaeftspartner]
> sortKB [] = []
> sortKB xs = [(foldr1 min xs)] ++ (sortKB (eintragENTF (foldr1 min xs) xs))
>     where eintragENTF _ [] = []
>           eintragENTF x (y:ys)
>              |  x == y = eintragENTF x ys 
>              |  otherwise = y : (eintragENTF x ys)



-------------------Test-Cases A.4    (Muster: (Geschaeftspartner,Geschaeftsvorfall) )

 kunde4 = (GP "Uwe" (D XXXI Nov 1969), Zahlung (C 273) DreiProzent (D XXXI Dez 2000))
 kunde5 = (GP "Dieter" (D XXXI Nov 10520), Zahlung (C 15024) KeinSkonto (D XXXI Dez 50123))
 kunde6 = (GP "Hans-Jurgen" (D XXIX Feb 2000), Zahlung (C 0) DreiProzent (D XXIX Feb 2001))
 kunde7 = (GP "Uwe" (D XXXI Nov 1964), Zahlung (C 273) DreiProzent (D XXXI Dez 1990))

 gp1 = (GP "Brian" (D XXXI Nov 1969))
 gp2 = (GP "Siegfried" (D XXXI Nov 10520))

 date1 = (D XXXI Nov 1969)
 date2 = (D I Jan 1978)

 test4 = (KB [kunde4, kunde5, kunde7, kunde4]) -- kunde5, kunde7, kunde4---------------------------------------------------------------------------------------------------------------
 testa4_2 = (GP "Mr Monopoly" (D XXXI Nov 10520), Zahlung (C 15024) FuenfProzent (D XXXI Dez 50123))
 testa4_3 = (GP "Mr Monopoly" (D XXIX Feb 2000), Zahlung (C 0) DreiProzent (D XXIX Feb 2001)) 

 type Kassabucheintrag = (Geschaeftspartner,Geschaeftsvorfall) 
 newtype Kassabuch = KB [Kassabucheintrag] deriving (Eq,Show)

------------------------------------------ Hilfsfunktionen
------------------- Funktionen Sortieren



Knapp, aber gut nachvollziehbar geht saldiere folgendermassen vor: 
...

