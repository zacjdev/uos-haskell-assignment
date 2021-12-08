{-
COM2108- Haskell Grading Assignment
Author: Zac Jones
stack ghci --package random
-}

-- Imports
import System.Random
import Data.List
import Data.Maybe
import Debug.Trace

-- Data types for Eight-off Solitaire
data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Eq, Show, Ord, Enum, Bounded)

data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Eq, Show, Ord, Enum, Bounded)

type Card = (Suit, Pip)

type Deck = [Card]

type Foundations = [Deck]

type Columns = [Deck]

type Reserves = Deck

data Board = Board1 EOBoard | Board2 SBoard

data EOBoard = EOBoard (Foundations, Columns, Reserves)

instance Show EOBoard where
  show (EOBoard (f, c, r)) = "Foundations:\n" ++ show f ++ "\nColumns:\n" ++ show c ++ "\nReserves:\n" ++ show r

instance Show Board where
  show (Board1 b) = show b
  show (Board2 b) = show b


-- Data types for Spider Solitaire

data SpiCard = SpiCard (Suit, Pip, Bool)

type SDeck = [SpiCard]

type SColumns = [SDeck]

type SFoundations = [SDeck]

type Stock = SDeck

data SBoard = SBoard (Stock , SColumns, SFoundations)

instance Show SBoard where
  show (SBoard (s, c, f)) = "Foundations:\n" ++ show f ++ "\nColumns:\n" ++ show c ++ "\nStock has " ++ show ((length s) `div` 10) ++ " deals left"

instance Show SpiCard where
  show (SpiCard (s, p, v)) = if v then show (s, p) else show "<unknown>"


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





compEOBoard :: Board
compEOBoard = Board1 (EOBoard ([[(s, p) | s <- [Hearts], p <- reverse [Ace .. King]], [(s, p) | s <- [Diamonds], p <- reverse [Ace .. King]], [(s, p) | s <- [Spades], p <- reverse [Ace .. King]], [(s, p) | s <- [Clubs], p <- reverse [Ace .. King]]], [], []))


-- Step 4: Implement further functionality
eODeal :: Int -> Board
eODeal n = Board1 (EOBoard (fs, cs, rs))
  where
    deck = shuffle n pack
    fs = []
    rs = drop 48 deck
    makeCols :: [Card] -> [Deck]
    -- split deck into decks of 6 cards
    makeCols xs = if length xs < 6 then [] else take 6 xs : makeCols (drop 6 xs)
    cs = makeCols deck




findAces :: Board -> Deck
findAces (Board1 (EOBoard (fs, cs, rs))) = filter isAce (possibleMoveableCards(Board1 (EOBoard (fs, cs, rs))))

findSuccs :: Board -> Deck
findSuccs (Board1 (EOBoard (fs, cs, rs)))
  | length fs == 0 = []
  | otherwise = trace ("SUCCS") (filter (\f -> f `elem` (possibleMoveableCards(Board1 (EOBoard (fs, cs, rs))))) (filter (not.null) (map sCard (map (\f -> last f) (filter (not.null) fs)))))

possibleMoveableCards :: Board -> [Card]
possibleMoveableCards (Board1 (EOBoard (fs, cs, rs))) = (map last (filter (not.null) cs)) ++ rs

possibleMoveableCardsNoReserves :: Board -> [Card]
possibleMoveableCardsNoReserves (Board1 (EOBoard (fs, cs, rs))) = (map last (filter (not.null) cs))

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


-- Part 6: Function to deal an opening SBoard

sPack :: [SpiCard]
sPack = [SpiCard (s, p, v) | s <- [Hearts .. Spades], p <- [Ace .. King], v <- [True]]


makeHidden :: SpiCard -> SpiCard
makeHidden (SpiCard (s, p, _)) = (SpiCard (s, p, False))

makeVisible :: SpiCard -> SpiCard
makeVisible (SpiCard (s, p, _)) = (SpiCard (s, p, True))




sDeal :: Int -> SBoard
sDeal n = SBoard (stock, columns, foundations)
  where
    deck = shuffle n sPack ++ shuffle (n+1) sPack
    foundations = []
    stock = take 50 deck
    makeSCols :: [SpiCard] -> [SDeck]
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
findMoves (Board1 (EOBoard (fs, cs, rs)))
  | (not.null) (cardsThatCanMoveToColumn (Board1 (EOBoard (fs, cs, rs)))) = (map (boardFromCardMoveToColumn (Board1 (EOBoard (fs, cs, rs)))) (cardsThatCanMoveToColumn (Board1 (EOBoard (fs, cs, rs)))))
  | (not.null) (kingsThatCanMoveToEmptyColumn (Board1 (EOBoard (fs, cs, rs)))) = (map (boardFromKingMoveToEmptyColumn (Board1 (EOBoard (fs, cs, rs)))) (kingsThatCanMoveToEmptyColumn (Board1 (EOBoard (fs, cs, rs)))))
  | (not.null) (cardsThatCanMoveToReserve (Board1 (EOBoard (fs, cs, rs)))) = (map (boardFromCardMoveToReserve (Board1 (EOBoard (fs, cs, rs)))) (cardsThatCanMoveToReserve (Board1 (EOBoard (fs, cs, rs)))))
  --length rs < 8 = map (boardFromCardMoveToReserve (Board1 (EOBoard (fs, cs, rs)))) (bestCardInColToMoveToRes (Board1 (EOBoard (fs, cs, rs))) : cardsThatCanMoveToReserve (Board1 (EOBoard (fs, cs, rs))))
  | otherwise = []

cardsThatCanMoveToColumn :: Board -> [Card]
cardsThatCanMoveToColumn (Board1 (EOBoard (fs, cs, rs))) = filter isNotAceKing (filter (\c -> c `elem` (possibleMoveableCards(Board1 (EOBoard (fs, cs, rs))))) (filter (not.null) (map pCard (map (\c -> last c) (filter (not.null)cs)))))

boardFromCardMoveToColumn :: Board -> Card -> Board
boardFromCardMoveToColumn (Board1 (EOBoard (fs, cs, rs))) card = (Board1 (EOBoard (fs', cs', rs')))
  where
    cs' = map (\c -> if length c == 0 then c else if sCard card == (last c) then c ++ [card] else c) (map (delete card) cs)
    rs' = delete card rs
    fs' = fs

kingsThatCanMoveToEmptyColumn :: Board -> [Card]
kingsThatCanMoveToEmptyColumn (Board1 (EOBoard (fs, cs, rs)))
  | (not.null) (filter (null) ((filter (\c -> not (isKing (last c) && length c == 1)) (filter (not.null) cs)))) = filter isKing (possibleMoveableCards(Board1 (EOBoard (fs, cs, rs))))
  | otherwise = []

boardFromKingMoveToEmptyColumn :: Board -> Card -> Board
boardFromKingMoveToEmptyColumn (Board1 (EOBoard (fs, cs, rs))) card = (Board1 (EOBoard (fs', cs', rs')))
  where
    fs' = fs
    cs' = (filter (not.null) (map (delete card) cs)) ++ [[card]]
    rs' = delete card rs

cardsThatCanMoveToReserve :: Board -> [Card]
cardsThatCanMoveToReserve (Board1 (EOBoard (fs, cs, rs)))
  | length rs < 8 = possibleMoveableCardsNoReserves(Board1 (EOBoard (fs, (filter (\c -> if length c == 1 then True else if length c == 2 then not (sCard (last c) == head c) else not (sCard (last c) == c!!((length c - 2)))) (filter (not.null)cs )), rs)))
  | otherwise = []

boardFromCardMoveToReserve :: Board -> Card -> Board
boardFromCardMoveToReserve (Board1 (EOBoard (fs, cs, rs))) card = (Board1 (EOBoard (fs', cs', rs')))
  where
  cs' = map (delete card) cs
  rs' = card : (delete card rs)
  fs' = fs

chooseMove :: Board -> Maybe Board
chooseMove board = if (not.null) (findMoves board) then Just (head (findMoves board)) else Nothing

haveWon :: Board -> Bool
haveWon board = (calculateScore board >= 52)

-- Initial board, uses chooseMove to play the game to completion. Returns score
playSolitaire :: Board -> Int
playSolitaire board
  | isNothing (chooseMove board) = calculateScore board
  | otherwise = trace ("debugging\n" ++ show board) (pS (toFoundations(fromJust (chooseMove board))))

-- seed, games to play, return 
analyseEO :: Int -> Int -> (Int, Double)
analyseEO seed games = (wins, avgscore)
  where
    scores = map playSolitaire (map eODeal [seed..seed+games])
    avgscore = (fromIntegral (sum scores)) / (fromIntegral games)
    wins = length (filter (==52) scores)

{-
Code designed to choose the best card to move to a reserve.

bestCardInColToMoveToRes :: Board -> Card
bestCardInColToMoveToRes board = bestCardToReserve' board 1
  where bestCardToReserve' :: Board -> Int -> Card
        bestCardToReserve' (Board1 (EOBoard (fs, cs, rs))) n
          -- give up, make random move
          | n > 3 = (last (head cs))
          | (not.null) (cardsWithMovesUnder (Board1 (EOBoard (fs, cs, rs))) n) = findCardToMoveFromCol (Board1 (EOBoard (fs, cs, rs))) (head (cardsWithMovesUnder (Board1 (EOBoard (fs, cs, rs))) n))
          | otherwise = bestCardToReserve' (Board1 (EOBoard (fs, cs, rs))) (n+1)

cardsWithMovesUnder :: Board -> Int -> [Card]
cardsWithMovesUnder board n = map fst (filter (\a -> (snd a) == True) (zip (possibleMoveableUnder board n) (map (checkCardUnder board) (possibleMoveableUnder board n))))

findCardToMoveFromCol :: Board -> Card -> Card
findCardToMoveFromCol (Board1 (EOBoard (fs, cs, rs))) card = last (head (filter (\c -> card `elem` c) cs))

checkCardUnder :: Board -> Card -> Bool
checkCardUnder (Board1 (EOBoard (fs, cs, rs))) card
-- is an empty column and the card is a king
  | (not.null) (filter (null) cs) && isKing card = True
-- card preceeds another card in a column
  | sCard card `elem` possibleMoveableCards(Board1 (EOBoard (fs, cs, rs))) = True
  | card `elem` (map sCard (map (\f -> last f) (filter (not.null) fs))) = True
  | otherwise = False


possibleMoveableUnder :: Board -> Int -> Deck
possibleMoveableUnder (Board1 (EOBoard (fs, cs, rs))) layer = (map (\c -> c!!(length c - layer)) (filter (\c -> length c >= layer) cs))
-}