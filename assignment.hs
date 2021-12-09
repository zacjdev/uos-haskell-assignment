{-
COM2108- Haskell Grading Assignment
Author: Zac Jones
stack ghci --package random
:load assignment.hs
main
analyseEO X Y
-}
-- Imports:
import System.Random
import Data.List
import Data.Maybe

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


-- Basic functionality for Eight-off Solitaire
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

-- Complete Eight-off board: Each foundation is full, columns and reserves are empty
compEOBoard :: Board
compEOBoard = Board1 (EOBoard ([[(s, p) | s <- [Hearts], p <- reverse [Ace .. King]], [(s, p) | s <- [Diamonds], p <- reverse [Ace .. King]], [(s, p) | s <- [Spades], p <- reverse [Ace .. King]], [(s, p) | s <- [Clubs], p <- reverse [Ace .. King]]], [], []))

-- Shuffle code from COM2108 Lecture 11 Slides, Author: Dr. Emma Norling
shuffle :: Int -> [a] -> [a]
shuffle n xs = [ x | (x, n) <- sortBy cmp (zip xs (randoms (mkStdGen n) :: [Int]))] -- Int seed
cmp (x1,y1) (x2,y2) = compare y1 y2

-- Appendix A board from Part 1, Step 3
initBoardEO :: Board
initBoardEO = Board1 (EOBoard (fs, cs, rs))
  where
    fs = []
    cs = [[(Spades,Four),(Clubs,King),(Hearts,Queen),(Hearts,Ace),(Diamonds,Seven),(Clubs,Ace)],
      [(Hearts,Seven),(Spades,Six),(Spades,Five),(Diamonds,Three),(Clubs,Queen),(Diamonds,Five)],
      [(Diamonds,Eight),(Hearts,Five),(Diamonds,Queen),(Spades,Seven),(Diamonds,Ten),(Hearts,King)],
      [(Spades,Queen),(Clubs,Ten),(Spades,Eight),(Clubs,Seven),(Hearts,Six),(Spades,Jack)],
      [(Clubs,Four),(Hearts,Jack),(Diamonds,King),(Diamonds,Ace),(Clubs,Eight),(Spades,Ace)],
      [(Clubs,Jack),(Diamonds,Six),(Hearts,Ten),(Clubs,Three),(Hearts,Three),(Diamonds,Two)],
      [(Spades,Ten),(Spades,Three),(Hearts,Nine),(Clubs,Nine),(Diamonds,Four),(Spades,Nine)],
      [(Hearts,Eight),(Spades,King),(Diamonds,Nine),(Hearts,Four),(Spades,Two),(Clubs,Two)]]
    rs = [(Hearts,Two),(Clubs,Six),(Clubs,Five),(Diamonds,Jack)]


-- Further functionality of Eight-off Solitaire
-- Deals a starting eight-off board using an integer seed.
-- fs = foundations, cs = columns, rs = reserves. They will be referred to like this throughout the program.
eODeal :: Int -> Board
eODeal n = Board1 (EOBoard (fs, cs, rs))
  where
    deck = shuffle n pack
    fs = []
    -- 4 cards into reserves
    rs = drop 48 deck
    makeCols :: [Card] -> [Deck]
    -- Splits deck into 8 coumns of 6 cards
    makeCols xs = if length xs < 6 then [] else take 6 xs : makeCols (drop 6 xs)
    cs = makeCols deck

-- Returns a list of the last card in each column
possibleMoveableCardsCols :: Board -> [Card]
possibleMoveableCardsCols (Board1 (EOBoard (fs, cs, rs))) = (map last (filter (not.null) cs))

-- Returns a list of the last card in each column plus the reserves
possibleMoveableCards :: Board -> Deck
possibleMoveableCards (Board1 (EOBoard (fs, cs, rs))) = (possibleMoveableCardsCols(Board1 (EOBoard (fs, cs, rs)))) ++ rs

-- Returns a list of aces in the last column cards and reserves
findAces :: Board -> Deck
findAces (Board1 (EOBoard (fs, cs, rs))) = filter isAce (possibleMoveableCards(Board1 (EOBoard (fs, cs, rs))))

-- Returns a list of foundation successors in the last column cards and reserves
findSuccs :: Board -> Deck
findSuccs (Board1 (EOBoard (fs, cs, rs)))
  | null fs = []
  | otherwise = (filter (\f -> f `elem` (possibleMoveableCards(Board1 (EOBoard (fs, cs, rs))))) (filter (not.null) (map sCard (map (\f -> last f) (filter (not.null) fs)))))

-- Takes a board and moves all the possible cards to the foundations
toFoundations :: Board -> Board
toFoundations (Board1 (EOBoard board))
  -- If there are aces to move, move them
  | (not.null) (findAces (Board1 (EOBoard board))) = toFoundations((
      moveToFoundation(head (findAces (Board1 (EOBoard board)))) (Board1 (EOBoard board))
        ))
  -- If there are successors to move, move them
  | (not.null) (findSuccs (Board1 (EOBoard board))) = toFoundations((
      moveToFoundation(head (findSuccs (Board1 (EOBoard board)))) (Board1 (EOBoard board))
      ))
  -- If there are no aces or successors to move, return the board
  | otherwise = (Board1 (EOBoard board))

-- Moves a card to a foundation
moveToFoundation :: Card -> Board -> Board
moveToFoundation card (Board1 (EOBoard (fs, cs, rs))) = (Board1 (EOBoard (fs', cs', rs')))
  where
    -- If the card is an ace, a new foundation is made. If not, it's matched to the correct suit and added to the start of that foundation.
    fs' = if isAce card then [card] : fs else map (\f -> if matchSuit card (head f) then card : f else f) fs
    -- Card is deleted from columns and reserves
    cs' = map (delete card) cs
    rs' = delete card rs

matchSuit :: Card -> Card -> Bool
matchSuit (s1,_) (s2,_) = s1 == s2


-- Find all possible boards from potential moves
findMoves :: Board -> [Board]
findMoves (Board1 (EOBoard (fs, cs, rs))) = 
  -- Boards from a move reserve or column to column
  (map (boardFromCardMoveToColumn (Board1 (EOBoard (fs, cs, rs)))) (cardsThatCanMoveToColumn (Board1 (EOBoard (fs, cs, rs))))) ++
  -- Boards from a king moving to an empty column
  (map (boardFromKingMoveToEmptyColumn (Board1 (EOBoard (fs, cs, rs)))) (kingsThatCanMoveToEmptyColumn (Board1 (EOBoard (fs, cs, rs))))) ++
  -- Boards from a column move to reserve
  (map (boardFromCardMoveToReserve (Board1 (EOBoard (fs, cs, rs)))) (cardsThatCanMoveToReserve (Board1 (EOBoard (fs, cs, rs)))))
  -- | length rs < 8 = map (boardFromCardMoveToReserve (Board1 (EOBoard (fs, cs, rs)))) (bestCardInColToMoveToRes (Board1 (EOBoard (fs, cs, rs))) : cardsThatCanMoveToReserve (Board1 (EOBoard (fs, cs, rs))))

-- Find the moveable cards that can be moved to a column
cardsThatCanMoveToColumn :: Board -> [Card]
cardsThatCanMoveToColumn (Board1 (EOBoard (fs, cs, rs))) = filter isNotAceKing (filter (\c -> c `elem` (possibleMoveableCards(Board1 (EOBoard (fs, cs, rs))))) (filter (not.null) (map pCard (map (\c -> last c) (filter (not.null)cs)))))

-- Return the resulting board from a card being moved to a column
boardFromCardMoveToColumn :: Board -> Card -> Board
boardFromCardMoveToColumn (Board1 (EOBoard (fs, cs, rs))) card = (Board1 (EOBoard (fs', cs', rs')))
  where
    cs' = map (\c -> if length c == 0 then c else if sCard card == (last c) then c ++ [card] else c) (map (delete card) cs)
    rs' = delete card rs
    fs' = fs

-- Function to help filter out aces and kings from being moved to the reserves to prevent infinite loops
isNotAceKing :: Card -> Bool
isNotAceKing (_, King) = False
isNotAceKing (_, Ace) = False
isNotAceKing _ = True

-- Find the moveable kings thatt can be moved tto a new column
kingsThatCanMoveToEmptyColumn :: Board -> [Card]
kingsThatCanMoveToEmptyColumn (Board1 (EOBoard (fs, cs, rs)))
  | (not.null) (filter (null) ((filter (\c -> not (isKing (last c) && length c == 1)) (filter (not.null) cs)))) = filter isKing (possibleMoveableCards(Board1 (EOBoard (fs, cs, rs))))
  | otherwise = []

-- Return the resulting board from a king being moved to an empty column
boardFromKingMoveToEmptyColumn :: Board -> Card -> Board
boardFromKingMoveToEmptyColumn (Board1 (EOBoard (fs, cs, rs))) card = (Board1 (EOBoard (fs', cs', rs')))
  where
    fs' = fs
    cs' = (filter (not.null) (map (delete card) cs)) ++ [[card]]
    rs' = delete card rs

-- Find the moveable cards that can be moved to a reserve
cardsThatCanMoveToReserve :: Board -> [Card]
cardsThatCanMoveToReserve (Board1 (EOBoard (fs, cs, rs)))
  | length rs < 8 = possibleMoveableCardsCols(Board1 (EOBoard (fs, (filter (\c -> if length c == 1 then True else if length c == 2 then not (sCard (last c) == head c) else not (sCard (last c) == c!!((length c - 2)))) (filter (not.null)cs )), rs)))
  | otherwise = []

-- Return the resulting board from a card being moved to a reserve
boardFromCardMoveToReserve :: Board -> Card -> Board
boardFromCardMoveToReserve (Board1 (EOBoard (fs, cs, rs))) card = (Board1 (EOBoard (fs', cs', rs')))
  where
  cs' = map (delete card) cs
  rs' = card : (delete card rs)
  fs' = fs

-- Chooses the first move in findMoves, as findMoves is ordered by priority
chooseMove :: Board -> Maybe Board
chooseMove board = if (not.null) (findMoves board) then Just (head (findMoves board)) else Nothing

-- If the score is 52, all foundations are full and the board is won
haveWon :: Board -> Bool
haveWon board = (calculateScore board >= 52)

-- Plays a game of solitaie using an integer seed
playSolitaire :: Board -> Int
playSolitaire board
  | isNothing (chooseMove board) = calculateScore board
  | otherwise = playSolitaire (toFoundations(fromJust (chooseMove board)))

-- Plays X games of solitaire using the initial seed and incrementing it. Returns number of wins and average score
analyseEO :: Int -> Int -> (Int, Double)
analyseEO seed games = (wins, avgscore)
  where
    scores = map playSolitaire (map eODeal [seed..seed+games])
    avgscore = (fromIntegral (sum scores)) / (fromIntegral games)
    wins = length (filter (==52) scores)

-- Calculates the score of a board state
calculateScore :: Board -> Int
calculateScore (Board1 (EOBoard (fs, cs, rs))) = sum (map length fs)


-- Basic functionality for Spider Solitaire

-- Full deck of spider solitaire cards, visible by default.
sPack :: [SpiCard]
sPack = [SpiCard (s, p, v) | s <- [Hearts .. Spades], p <- [Ace .. King], v <- [True]]

-- Functions to hide or reveal a spider card.
makeHidden :: SpiCard -> SpiCard
makeHidden (SpiCard (s, p, _)) = (SpiCard (s, p, False))

makeVisible :: SpiCard -> SpiCard
makeVisible (SpiCard (s, p, _)) = (SpiCard (s, p, True))

-- Function to deal a full starting spider board using an integer seed.
sDeal :: Int -> SBoard
sDeal n = SBoard (stock, columns, foundations)
  where
    -- There are 2 decks in spider solitaire
    deck = shuffle n sPack ++ shuffle (n+1) sPack
    foundations = []
    stock = take 50 deck
    makeSCols :: [SpiCard] -> [SDeck]
    -- Split deck into decks of 6 cards. 4x6cards, 6x5cards
    makeSCols xs
      | length xs < 5 = []
      -- 4 piles of 6 cards
      | length xs > 30 = ((map (makeHidden) (take 5 xs)) ++ take 1 xs) : makeSCols (drop 6 xs)
      -- 6 piles of 5 cards
      | otherwise = ((map (makeHidden) (take 4 xs)) ++ take 1 xs) : makeSCols (drop 5 xs)
    columns = makeSCols (drop 50 deck)

-- I've replaced the unknown cards with (Hearts,Ace,False) as there is no way of me knowing what each card im the screenshot is.
initBoardS :: Board
initBoardS = Board2 (SBoard (stock, columns, foundations))
  where
    stock = [SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True)),
      SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Ace,True))]
    columns = [[SpiCard((Diamonds,Eight,True)),SpiCard((Hearts,Nine,True))],
      [SpiCard((Diamonds,Two,True))],
      [SpiCard((Spades,Ace,True)),SpiCard((Spades,Two,True)),SpiCard((Spades,Three,True)),SpiCard((Spades,Four,True)),SpiCard((Spades,Five,True)),SpiCard((Clubs,Six,True)),SpiCard((Clubs,Seven,True)),SpiCard((Clubs,Eight,True)),SpiCard((Clubs,Nine,True)),SpiCard((Diamonds,Ten,True)),SpiCard((Diamonds,Jack,True)),SpiCard((Diamonds,Queen,True)),SpiCard((Diamonds,King,True)),SpiCard((Hearts,Ace,False)),SpiCard((Hearts,Ace,False))],
      [SpiCard((Clubs,Seven,True)),SpiCard((Diamonds,Eight,True)),SpiCard((Diamonds,Nine,True)),SpiCard((Diamonds,Ten,True)),SpiCard((Diamonds,Jack,True)),SpiCard((Diamonds,Queen,True)),SpiCard((Diamonds,King,True)),SpiCard((Clubs,Nine,True)),SpiCard((Hearts,Ten,True)),SpiCard((Clubs,Jack,True))],
      [SpiCard((Hearts,Ace,True)),SpiCard((Hearts,Two,True)),SpiCard((Hearts,Three,True)),SpiCard((Hearts,Four,True)),SpiCard((Hearts,Five,True)),SpiCard((Diamonds,Six,True)),SpiCard((Diamonds,Seven,True)),SpiCard((Clubs,Queen,True)),SpiCard((Hearts,King,True))],
      [SpiCard((Diamonds,Two,True)),SpiCard((Diamonds,Three,True)),SpiCard((Diamonds,Four,True))],
      [SpiCard((Clubs,Jack,True)),SpiCard((Clubs,Queen,True)),SpiCard((Clubs,King,True)),SpiCard((Spades,Two,True)),SpiCard((Spades,Three,True)),SpiCard((Diamonds,Four,True)),SpiCard((Diamonds,Five,True)),SpiCard((Diamonds,Six,True)),SpiCard((Hearts,Seven,True)),SpiCard((Clubs,Eight,True)),SpiCard((Spades,Nine,True)),SpiCard((Clubs,Ten,True)),SpiCard((Clubs,Ace,True)),SpiCard((Clubs,Two,True)),SpiCard((Clubs,Three,True)),SpiCard((Clubs,Four,True)),SpiCard((Spades,Five,True))],
      [SpiCard((Spades,Seven,True)),SpiCard((Spades,Eight,True)),SpiCard((Spades,Nine,True)),SpiCard((Spades,Ten,True)),SpiCard((Spades,Jack,True)),SpiCard((Spades,Queen,True)),SpiCard((Spades,King,True)),SpiCard((Hearts,Ace,False)),SpiCard((Hearts,Ace,False)),SpiCard((Hearts,Ace,False))],
      [SpiCard((Hearts,Jack,True)),SpiCard((Hearts,Queen,True))],
      [SpiCard((Clubs,Ace,True)),SpiCard((Clubs,Two,True))]]
    foundations = [[SpiCard((Hearts,King,True))]]

{-
Code designed to choose the best card to move to a reserve for EO solitaire

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


{- Paste the contents of this file, including this comment, into your source file, below all
    of your code. You can change the indentation to align with your own, but other than this,
    ONLY make changes as instructed in the comments.
  -}
-- Constants that YOU must set:
studentName = "Zac Jones"
studentNumber = "200130637"
studentUsername = "sma19zj"

initialBoardDefined = initBoardEO {- replace XXX with the name of the constant that you defined
                              in step 3 of part 1 -}
secondBoardDefined = initBoardS {- replace YYY with the constant defined in step 5 of part 1,
                            or if you have chosen to demonstrate play in a different game
                            of solitaire for part 2, a suitable contstant that will show
                            your play to good effect for that game -}

{- Beyond this point, the ONLY change you should make is to change the comments so that the
    work you have completed is tested. DO NOT change anything other than comments (and indentation
    if needed). The comments in the template file are set up so that only the constant eight-off
    board from part 1 and the toFoundations function from part 1 are tested. You will probably
    want more than this tested.

    CHECK with Emma or one of the demonstrators if you are unsure how to change this.

    If you mess this up, your code will not compile, which will lead to being awarded 0 marks
    for functionality and style.
-}

main :: IO()
main =
  do
    putStrLn $ "Output for " ++ studentName ++ " (" ++ studentNumber ++ ", " ++ studentUsername ++ ")"

    putStrLn "***The eight-off initial board constant from part 1:"
    print initialBoardDefined

    let board = toFoundations initialBoardDefined
    putStrLn "***The result of calling toFoundations on that board:"
    print board

    {- Move the start comment marker below to the appropriate position.
      If you have completed ALL the tasks for the assignment, you can
      remove the comments from the main function entirely.
      DO NOT try to submit/run non-functional code - you will receive 0 marks
      for ALL your code if you do, even if *some* of your code is correct.
    -}

    -- start comment marker - move this if appropriate

    let boards = findMoves board      -- show that findMoves is working
    putStrLn "***The possible next moves after that:"
    print boards

    let chosen = chooseMove board     -- show that chooseMove is working
    putStrLn "***The chosen move from that set:"
    print chosen

    putStrLn "***Now showing a full game"     -- display a full game
    score <- displayGame initialBoardDefined 0
    putStrLn $ "Score: " ++ score
    putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire initialBoardDefined)


    putStrLn "\n\n\n************\nNow looking at the alternative game:"

    putStrLn "***The spider initial board constant from part 1 (or equivalent if playing a different game of solitaire):"
    print secondBoardDefined          -- show the suitable constant. For spider solitaire this
                                      -- is not an initial game, but a point from which the game
                                      -- can be won
    {-
    putStrLn "***Now showing a full game for alternative solitaire"
    score <- displayGame secondBoardDefined 0 -- see what happens when we play that game (assumes chooseMove
                                              -- works correctly)
    putStrLn $ "Score: " ++ score
    putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire secondBoardDefined)

    -}

{- displayGame takes a Board and move number (should initially be 0) and
    displays the game step-by-step (board-by-board). The result *should* be
    the same as performing playSolitaire on the initial board, if it has been
    implemented correctly.
    DO NOT CHANGE THIS CODE other than aligning indentation with your own.
-}
displayGame :: Board -> Int ->IO String
displayGame board n =
  if haveWon board
    then return "A WIN"
    else
      do
        putStr ("Move " ++ show n ++ ": " ++ show board)
        let maybeBoard = chooseMove board
        if isJust maybeBoard then
          do
            let (Just newBoard) = maybeBoard
            displayGame newBoard (n+1)
        else
          do
            let score = show (playSolitaire board)
            return score
