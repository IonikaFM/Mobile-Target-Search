{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = Node
    { state :: s
    , action :: Maybe a
    , parent :: Maybe (Node s a)
    , depth :: Int
    , cost :: Float
    , children :: [Node s a]
    }

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    (Node state1 _ _ _ _ _) == (Node state2 _ _ _ _ _) = state1 == state2


instance Ord s => Ord (Node s a) where
    (Node state1 _ _ _ _ _) <= (Node state2 _ _ _ _ _) = state1 <= state2

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState = state

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent = parent

nodeDepth :: Node s a -> Int
nodeDepth = depth

nodeChildren :: Node s a -> [Node s a]
nodeChildren = children

nodeHeuristic :: Node s a -> Float
nodeHeuristic = cost

nodeAction :: Node s a -> Maybe a
nodeAction = action

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = createStateSpaceHelper Nothing Nothing initialState 0

createStateSpaceHelper :: (ProblemState s a, Eq s) => Maybe (Node s a) -> Maybe a -> s -> Int -> Node s a
createStateSpaceHelper myparent myaction mystate mydepth = currentNode
    where
        currentNode = Node mystate myaction myparent mydepth (h mystate) kids
        kids = map (\x -> createStateSpaceHelper (Just currentNode) (Just (fst x)) (snd x) (mydepth + 1)) (successors mystate)

{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => (PQ.PSQ k p) -> (k, (PQ.PSQ k p))
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}

suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> (S.Set s) -> [Node s a]
suitableSuccs node visited =
    foldr (\x acc -> (if nodeState x `notElem` visited then x : acc else acc)) [] (nodeChildren node)


{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}

insertSucc :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node = PQ.insertWith min node (nodeHeuristic node + fromIntegral (nodeDepth node)) frontier

{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insertSuccs :: (ProblemState s a, Ord s) => (Node s a) -> (PQ.PSQ (Node s a) Float) -> (S.Set s) -> (PQ.PSQ (Node s a) Float)
insertSuccs node frontier visited = foldr (\x acc -> insertSucc acc x) frontier (suitableSuccs node visited)

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

astar' :: (ProblemState s a, Ord s) => (S.Set s) -> (PQ.PSQ (Node s a) Float) -> Node s a
astar' visited frontier = if isGoal (state minNode)
    then minNode
    else astar' newVisited (insertSuccs minNode newFrontier newVisited)
        where
            pair = deleteFindMin frontier
            minNode = fst pair
            newFrontier = snd pair
            newVisited = S.insert (state minNode) visited
{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = astar' S.empty (insertSucc PQ.empty initialNode)

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

extractPath :: (ProblemState s a, Ord s) => Node s a -> [(a, s)]
extractPath goalNode = map (\x -> (fromJust (nodeAction (fromJust x)), nodeState (fromJust x))) nodeList
    where
        nodeList = tail (reverse (takeWhile (/= Nothing) (iterate (nodeParent . fromJust) (Just goalNode))))