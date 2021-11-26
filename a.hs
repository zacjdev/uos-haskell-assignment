{-
Haskell Grading Assignment
Author: Zac Jones (unless otherwise stated)
stack ghci --package random
-}

-- Step 1: Initial Datatypes
import System.Random
import Data.List
import Data.Bool (Bool)

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

type Reserves = Deck

newtype Board = Board EOBoard-- | Board (SBoard)
  deriving (Show)

type EOBoard = (Foundations, Columns, Reserves)

compBoard :: EOBoard
compBoard = ([[(s, p) | s <- [Hearts], p <- reverse [Ace .. King]], [(s, p) | s <- [Diamonds], p <- reverse [Ace .. King]], [(s, p) | s <- [Spades], p <- reverse [Ace .. King]], [(s, p) | s <- [Clubs], p <- reverse [Ace .. King]]], [], [])

testBoard :: EOBoard
testBoard = ([[(Hearts, Ace)],[],[],[]], [], [(Hearts, Two)])

-- Step 4: Implement further functionality
eODeal :: Int -> EOBoard
eODeal n = (foundations, columns, reserves)
  where
    deck = shuffle n pack
    foundations = []
    reserves = take 4 deck
    makeCols :: [Card] -> [Deck]
    -- split deck into decks of 6 cards
    makeCols xs = if length xs < 6 then [] else take 6 xs : makeCols (drop 6 xs)
    columns = makeCols deck




findAces :: EOBoard -> Deck
findAces (fs,cs,rs) = filter isAce ((map head (filter (not.null) cs)) ++ rs) -- COPIED CODE!!!!!

findSuccs :: EOBoard -> Deck
findSuccs (fs,cs,rs) = filter (\f -> f `elem` ((map head (filter (not.null) cs)) ++ rs)) (filter (not.null) (map sCard (map (\f -> last f) fs))) -- COPIED CODE!!!!!

toFoundations :: EOBoard -> EOBoard
toFoundations board
  | (not.null) (findAces board) = toFoundations(moveToFoundation(head (findAces board)) board)
  | (not.null) (findSuccs board) = toFoundations(moveToFoundation(head (findSuccs board)) board)
  | otherwise = board

moveToFoundation :: Card -> EOBoard -> EOBoard
moveToFoundation card (fs,cs,rs) = (fs',cs',rs')
  where
    fs' = if isAce card then [card] : fs else map (\f -> if matchSuit card (head f) then card : f else f) fs
    cs' = map (delete card) cs
    rs' = delete card rs

matchSuit :: Card -> Card -> Bool
matchSuit (s1,_) (s2,_) = s1 == s2

