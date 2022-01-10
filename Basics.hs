{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.Maybe

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Game = Game
    { line :: Int
    , col :: Int
    , hunter :: Position
    , targets :: [Target]
    , obstacles :: [Position]
    , gateways :: [(Position, Position)]
    } deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
gameAsString :: Game -> String
gameAsString game = [returnChar x y game |x <- [0..line game-1], y <- [0..(col game)], x /= line game - 1 || y /= col game]

returnChar :: Int -> Int -> Game -> Char
returnChar n m game
    | (n, m) == hunter game = '!'
    | (n, m) `elem` map position (targets game) = '*'
    | (n, m) `elem` obstacles game = '@'
    | (n, m) `elem` map fst (gateways game) ++ map snd (gateways game) = '#'
    | m == col game && n /= line game-1 = '\n'
    | otherwise = ' '

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame n m = Game n m (1, 1) [] [(x, y) | x <- [0..n-1], y <- [0..m-1], x == 0 || x == n-1 || y == 0 || y == m -1] []

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter (n, m) game = if n >= line game || m >= col game || n <= 0 || m <= 0 || returnChar n m game /= ' '
    then game
    else Game (line game) (col game) (n, m) (targets game) (obstacles game) (gateways game)

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget behav pos game = Game (line game) (col game) (hunter game) (Target pos behav : targets game) (obstacles game) (gateways game)

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway gateway game = Game (line game) (col game) (hunter game) (targets game) (obstacles game) (gateway : gateways game)

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle pos game = Game (line game) (col game) (hunter game) (targets game) (pos : obstacles game) (gateways game)

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove (n, m) game
    | returnChar n m game == ' ' = Just (n, m)
    | returnChar n m game == '#' = Just (searchPair (n, m) (gateways game))
    | returnChar n m game == '@' = Nothing
    | otherwise = Nothing

searchPair :: (Num a, Num b, Eq a, Eq b) => (a, b) -> [((a, b), (a, b))] -> (a, b)
searchPair _ [] = (-1, -1)
searchPair pos mygateways
    | fst (head mygateways) == pos = snd (head mygateways)
    | snd (head mygateways) == pos = fst (head mygateways)
    | otherwise = searchPair pos (tail mygateways)
{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}
goSomewhere :: (Int, Int) -> Behavior
goSomewhere direction pos game = if isNothing next
    then if pair == (-1, -1)
        then Target pos (goSomewhere direction)
        else Target pair (goSomewhere direction)
    else Target (fromJust next) (goSomewhere direction)
        where
            next = attemptMove (((fst pos) + (fst direction)) ,  ((snd pos) + (snd direction))) game
            pair = searchPair pos (gateways game)

goEast :: Behavior
goEast = goSomewhere (0, 1)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest = goSomewhere (0, -1)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth = goSomewhere (-1, 0)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth = goSomewhere (1, 0)

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
bounce :: Int -> Behavior
bounce direction pos game = if isNothing newPos
    then if isJust otherPos
        then Target (fromJust otherPos) (bounce (direction * (-1)))
        else Target pos (bounce (direction * (-1)))
    else Target (fromJust newPos) (bounce direction)
        where
            newPos = attemptMove (fst pos + direction, snd pos) game
            otherPos = attemptMove (fst pos - direction, snd pos) game

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
moveTargets :: Game -> Game
moveTargets game = Game (line game) (col game) (hunter game) (map (\t -> behavior t (position t) game) (targets game)) (obstacles game) (gateways game)

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled myhunter target
    | fst myhunter + 1 == fst (position target) && snd myhunter == snd (position target) = True
    | fst myhunter - 1 == fst (position target) && snd myhunter == snd (position target) = True
    | fst myhunter == fst (position target) && snd myhunter + 1 == snd (position target) = True
    | fst myhunter == fst (position target) && snd myhunter - 1 == snd (position target) = True
    | otherwise = False


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
getPosFromDirection :: (Num a, Num b) => Direction -> (a, b)
getPosFromDirection direction
    | direction == North = (-1, 0)
    | direction == South = (1, 0)
    | direction == East = (0, 1)
    | otherwise = (0, -1)

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir bool game = if not bool
    then Game (line game) (col game) newHunter (targets game) (obstacles game) (gateways game)
    else Game (line game) (col game) newHunter targetsAfterSecondKill (obstacles game) (gateways game)
        where
            newHunter = fromMaybe (hunter game) (attemptMove newPos game)
            newPos = (fst direction + fst (hunter game), snd direction + snd (hunter game))
            direction = getPosFromDirection dir
            targetsAfterFirstKill = eliminateAdiacentTargets (targets game) newHunter
            targetsMoved = targets (moveTargets gameAfterFirstKill)
            targetsAfterSecondKill = eliminateAdiacentTargets targetsMoved newHunter
            gameAfterFirstKill = Game (line game) (col game) newHunter targetsAfterFirstKill (obstacles game) (gateways game)

eliminateAdiacentTargets :: [Target] -> Position -> [Target]
eliminateAdiacentTargets [] _ = []
eliminateAdiacentTargets mytargets myhunter = if isTargetKilled myhunter (head mytargets)
    then eliminateAdiacentTargets (tail mytargets) myhunter
    else head mytargets : eliminateAdiacentTargets (tail mytargets) myhunter

{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game = null (targets game)

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined

canKill :: [Target] -> Position -> Maybe Position
canKill [] _ = Nothing
canKill mytargets myhunter = if isTargetKilled myhunter (head mytargets)
    then Just (position (head mytargets))
    else canKill (tail mytargets) myhunter

instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = north ++ south ++ east ++ west
        where
            east = [(East, advanceGameState East False game)]
            west = [(West, advanceGameState West False game)]
            north = [(North, advanceGameState North False game)]
            south = [(South, advanceGameState South False game)]

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}

    isGoal game = isJust (canKill (targets game) (hunter game))

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h game = hEuclidean (hunter game) (fromJust (canKill (targets game) (hunter game)))

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ (x1 - x2) ^ pow + (y1 - y2) ^ pow
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
