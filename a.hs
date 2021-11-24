{-
Haskell Grading Assignment
Author: Zac Jones (unless otherwise stated)
stack ghci --package random
-}

-- Step 1: Initial Datatypes
import System.Random
import Data.List

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Eq, Show, Ord, Enum, Bounded)

data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Eq, Show, Ord, Enum, Bounded)

type Card = (Suit, Pip)

type Deck = [Card]

-- Step 2: Basic functionality
pack :: [Card]
pack = [(s, p) | s <- [Hearts .. Spades], p <- [Ace .. King]]

sCard :: Card -> Card
sCard (s, King) = (s, Ace)
sCard (s, p) = (s, succ p)

pCard :: Card -> Card
pCard (s, Ace) = (s, King)
pCard (s, p) = (s, pred p)

isAce :: Card -> Bool
isAce (s, p) = p == Ace

isKing :: Card -> Bool
isKing (s, p) = p == King

-- code adapted from (slide)
shuffle :: Int -> [a] -> [a]
shuffle n xs = [ x | (x, n) <- sortBy cmp (zip xs (randoms (mkStdGen n) :: [Int]))] -- Int seed
cmp (x1,y1) (x2,y2) = compare y1 y2

-- Step 3: Types to represent a board

type Foundations = [Deck]

type Columns = [Deck]

type Reserves = [Card]

newtype Board = Board EOBoard-- | Board (SBoard)
  deriving (Show)

newtype EOBoard = EOBoard (Foundations, Columns, Reserves)

instance Show EOBoard where
  show (EOBoard (fs, cs, rs)) = "Foundations: " ++ show fs ++ "\nColumns: " ++ show cs ++ "\nReserves: " ++ show rs


-- Step 4: Implement further functionality
eODeal :: Int -> EOBoard
eODeal n = EOBoard (foundations, columns, reserves)
  where
    deck = shuffle n pack
    foundations = [[],[],[],[]]
    reserves = take 4 deck
    makeCols :: [Card] -> [Deck]
    -- split deck into decks of 6 cards
    makeCols xs = if length xs < 6 then [] else take 6 xs : makeCols (drop 6 xs)
    columns = makeCols deck

toFoundations :: EOBoard -> EOBoard
toFoundations board@(fs, c:cs, rs)
  | foundationValid tail c fs = True

foundationValid :: Card -> Foundations -> Bool
foundationValid card (f:fs)
  | null f = False
  | isAce card = True
  | card == sCard(last f) = True
  | otherwise = foundationValid card fs


--stack ghci --package random

{-
-- foundation will add new elements to the end. i0 = ace
foundationValid :: Card -> Foundation -> Bool
foundationValid c fs
  | null c = False
  | isAce c = True
  | c `elem` filter fs (sCard . head) = True
  | otherwise  = False

findCardsToMove :: EOBoard -> Deck
findCardsToMove (fs, cs, rs) =
  filter foundationValid fs (filter (not.null) (map head columns ++ rs))



-}



{-}
toFoundations :: EOBoard -> EOBoard
toFoundations board = toFoundations' board EOBoard {foundations = foundations board, columns = columns board, reserve = reserve board}
  where
      --                temp       save to    output
    toFoundations' :: EOBoard -> EOBoard -> EOBoard
    toFoundations' board@(EOBoard fs cs rs) board'@(EOBoard fs' cs' rs')
      -- if all cs checked, return board'
      | null cs = board'
      -- if no fs left to check, reset fs and try nxt c
      | null fs = toFoundations' (EOBoard (fs') (tail cs) rs) board' 
       --(EOBoard (fs' (init cs) rs)) board'
      -- if f c valid, remove last of column and add to foundation, reset fs, next c
      | foundationValid (last cs) (head fs) = toFoundations' (EOBoard fs' (tail cs) rs) (EOBoard (moveToFoundations fs' head fs (last cs)) (init . last cs) rs)
      -- if f c not valid, remove f and try again
      | otherwise = toFoundations' (EOBoard (init fs) cs rs) board'
-}
{-}

-}
{-}
foundationValid :: [Foundation] -> Column -> Bool
foundationValid (f:fs) col
  | null col = False
  | isAce(tail col) && null fs = True
  | pCard(tail col) == tail foundation = True
  | null fs = False
  | otherwise = foundationValid fs col

foundationValid col f
  | null col || null f = False
  | isAce(tail col) && null f = True
  | pCard(tail col) == tail f = True
  | otherwise  = False
-}
-- iterate foundation suits

--foundations board

--data SBoard = 
