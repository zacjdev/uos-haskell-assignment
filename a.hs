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

type Reserves = Deck


data SCard = SCard (Suit, Pip, Bool)

type SDeck = [SCard]

makeHidden :: SCard -> SCard
makeHidden (SCard (s, p, _)) = (SCard (s, p, False))

makeVisible :: SCard -> SCard
makeVisible (SCard (s, p, _)) = (SCard (s, p, True))


instance Show SCard where
  show (SCard (s, p, v)) = if v then show (s, p) else show "<unknown>"


type SColumns = [SDeck]

type SFoundations = [SDeck]

type Stock = SDeck

data SBoard = SBoard (Stock , SColumns, SFoundations)

instance Show SBoard where
  show (SBoard (s, c, f)) = "Foundations:\n" ++ show f ++ "\nColumns:\n" ++ show c ++ "\nStock has " ++ show ((length s) `div` 10) ++ " deals left"

data Board = Board1 EOBoard | Board2 SBoard

data EOBoard = EOBoard (Foundations, Columns, Reserves)

instance Show EOBoard where
  show (EOBoard (f, c, r)) = "Foundations:\n" ++ show f ++ "\nColumns:\n" ++ show c ++ "\nReserves:\n" ++ show r

instance Show Board where
  show (Board1 b) = show b
  show (Board2 b) = show b


compEOBoard :: Board
compEOBoard = Board1 (EOBoard ([[(s, p) | s <- [Hearts], p <- reverse [Ace .. King]], [(s, p) | s <- [Diamonds], p <- reverse [Ace .. King]], [(s, p) | s <- [Spades], p <- reverse [Ace .. King]], [(s, p) | s <- [Clubs], p <- reverse [Ace .. King]]], [], []))


-- Step 4: Implement further functionality
eODeal :: Int -> Board
eODeal n = Board1 (EOBoard (fs, cs, rs))
  where
    deck = shuffle n pack
    fs = []
    rs = take 4 deck
    makeCols :: [Card] -> [Deck]
    -- split deck into decks of 6 cards
    makeCols xs = if length xs < 6 then [] else take 6 xs : makeCols (drop 6 xs)
    cs = makeCols deck




findAces :: Board -> Deck
findAces (Board1 (EOBoard (fs, cs, rs))) = filter isAce (possibleMoveableCards(Board1 (EOBoard (fs, cs, rs))))

findSuccs :: Board -> Deck
findSuccs (Board1 (EOBoard (fs, cs, rs))) = filter (\f -> f `elem` (possibleMoveableCards(Board1 (EOBoard (fs, cs, rs))))) (filter (not.null) (map sCard (map (\f -> last f) fs)))

possibleMoveableCards :: Board -> [Card]
possibleMoveableCards (Board1 (EOBoard (fs, cs, rs))) = map last (filter (not.null) cs) ++ rs

toFoundations :: Board -> Board
toFoundations (Board1 (EOBoard board))
  | (not.null) (findAces (Board1 (EOBoard board))) = toFoundations((
      moveToFoundation(head (findAces (Board1 (EOBoard board)))) (Board1 (EOBoard board))
        ))
  | (not.null) (findSuccs (Board1 (EOBoard board))) = toFoundations((
      moveToFoundation(head (findSuccs (Board1 (EOBoard board)))) (Board1 (EOBoard board))
      ))
  | otherwise = (Board1 (EOBoard board))

moveToFoundation :: Card -> Board -> Board
moveToFoundation card (Board1 (EOBoard (fs, cs, rs))) = (Board1 (EOBoard (fs', cs', rs')))
  where
    fs' = if isAce card then [card] : fs else map (\f -> if matchSuit card (head f) then card : f else f) fs
    cs' = map (delete card) cs
    rs' = delete card rs

matchSuit :: Card -> Card -> Bool
matchSuit (s1,_) (s2,_) = s1 == s2

-- Step 5: Datatypes for spider solitaire


sPack :: [SCard]
sPack = [SCard (s, p, v) | s <- [Hearts .. Spades], p <- [Ace .. King], v <- [True]]


-- Part 6: Function to deal an opening SBoard
sDeal :: Int -> SBoard
sDeal n = SBoard (stock, columns, foundations)
  where
    deck = shuffle n sPack ++ shuffle (n+1) sPack
    foundations = []
    stock = take 50 deck
    makeSCols :: [SCard] -> [SDeck]
    -- split deck into decks of 6 cards. 4x6cards, 6x5cards
    makeSCols xs
      | length xs < 6 = []
      | length xs > 24 = ((map (makeHidden) (take 5 xs)) ++ take 1 xs) : makeSCols (drop 6 xs)
      | otherwise = ((map (makeHidden) (take 4 xs)) ++ take 1 xs) : makeSCols (drop 5 xs)
    columns = makeSCols deck

-- Part 2, Step 1: All possible moves for Eight-Off


--, boardsFromMoveToReserve(Board1 (EOBoard (fs, cs, rs)))]

-- add just the result of toFoundations

-- cards that can move to a column function (from column or reserve) - INCLUDES KINGS


isNotAceKing :: Card -> Bool
isNotAceKing (_, King) = False
isNotAceKing (_, Ace) = False
isNotAceKing _ = True

-- Algorithm:
--See if to foundations does anything, if so do itt
--Find cards that can move from a column or reserve to another column, and do it. Check for infinite loop.

-- Cards that can move from a reserve or column onto another column


-- execute boardFromCardMoveToColumn on each result from cardsThatCanMoveToColumn
findMoves :: Board -> [Board]
findMoves board = map (boardFromCardMoveToColumn board) (cardsThatCanMoveToColumn board) ++ map (boardFromCardMoveToReserve board) (cardsThatCanMoveToReserve board) ++ map (boardFromKingMoveToEmptyColumn board) (kingsThatCanMoveToEmptyColumn board)


cardsThatCanMoveToColumn :: Board -> [Card]
cardsThatCanMoveToColumn (Board1 (EOBoard (fs, cs, rs))) = filter isNotAceKing (filter (\c -> c `elem` (possibleMoveableCards(Board1 (EOBoard (fs, cs, rs))))) (filter (not.null) (map pCard (map (\c -> last c) cs))))

boardFromCardMoveToColumn :: Board -> Card -> Board
boardFromCardMoveToColumn (Board1 (EOBoard (fs, cs, rs))) card = (Board1 (EOBoard (fs', cs', rs')))
  where
    cs' = map (\c -> if sCard card == (last c) then c ++ [card] else c) (map (delete card) cs)
    rs' = delete card rs
    fs' = fs

kingsThatCanMoveToEmptyColumn :: Board -> [Card]
kingsThatCanMoveToEmptyColumn (Board1 (EOBoard (fs, cs, rs)))
  | (not.null) (filter (null) cs) = filter isKing (possibleMoveableCards(Board1 (EOBoard (fs, cs, rs))))
  | otherwise = []

boardFromKingMoveToEmptyColumn :: Board -> Card -> Board
boardFromKingMoveToEmptyColumn (Board1 (EOBoard (fs, cs, rs))) card = (Board1 (EOBoard (fs', cs', rs')))
  where
    fs' = fs
    cs' = (filter (not.null) (map (delete card) cs)) ++ [[card]]
    rs' = delete card rs

cardsThatCanMoveToReserve :: Board -> [Card]
cardsThatCanMoveToReserve (Board1 (EOBoard (fs, cs, rs)))
  | length rs < 8 = possibleMoveableCards(Board1 (EOBoard (fs, cs, rs)))
  | otherwise = []

boardFromCardMoveToReserve :: Board -> Card -> Board
boardFromCardMoveToReserve (Board1 (EOBoard (fs, cs, rs))) card = (Board1 (EOBoard (fs', cs', rs')))
  where
  cs' = map (delete card) cs
  rs' = if length rs < 8 then card : (delete card rs) else rs
  fs' = fs


-- Move king to empty
-- Filter for boards after moving last of column to a foundation where a move could happen
-- If none, filter for boards aafter moving last 2, then 3, etc of column to foundatiton could do something useful




{-}
map (\c -> if pCard card (last c) then c : card else c) cs


boardFromMoveToColumn :: Board -> Card -> Board
boardFromMoveToColumn (Board (EOBoard (fs, cs, rs))) card = Board (EOBoard (fs, cs', rs'))
  where
    cs' = map (delete card) cs
    rs' = delete card rs


possibleMoveableCards (Board (EOBoard (fs, cs, rs))) -- returns [Card]

-- iterate, see if the card can move to a column


-}



-- if yes, get resulting board and add it

-- stack ghci --package random
-- stack ghci --package random




-- cards that can move to a foundation function
-- move kings to empty column first


--boardsFromMoveToReserve :: Board -> [Board]
--boardsFromMoveToReserve (Board (EOBoard (fs, cs, rs))) = []
---- all possible combinations
-- no moving solo king



-- stack ghci --package random

{-
-- Part 2, Step 2: Function to choose next move
chooseMove :: Board -> Maybe Board
chooseMove b
  | null findMoves b = Nothing
  | otherwise = chooseMove' findMoves b
    where
      chooseMove' :: [Board] -> Board -> Maybe Board
      chooseMove' [] _ = Nothing
      chooseMove' (x:xs) b
        | x == b = Just x
        | otherwise = chooseMove' xs b

-- if board == findmoves board = nothing



-- Part 2, Step 3: Function to play game of Eight-off
haveWon :: Board -> Bool 
-- equal compBoard

playSolitaire :: Board -> Int

-- may not be needed?
countFoundations :: Board -> Int

-- Part 2, Step 4: Analyse performance

-- initial seed (iterate each time), number of games, return number of wins
analyseEO :: Int -> Int -> Int


-- Part 2, Step 5: Implement spider solitaire

-}
