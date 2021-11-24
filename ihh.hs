data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Eq, Show, Ord, Enum, Bounded)

data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Eq, Show, Ord, Enum, Bounded)

newtype Card = Card (Suit, Pip)
  deriving (Eq, Show)

newtype Deck = Deck [Card]
  deriving (Eq, Show)

newtype Foundation = Foundation [Card]
  deriving (Eq, Show)

newtype Column = Column [Card]
  deriving (Eq, Show)

newtype Reserve = Reserve [Card]
  deriving (Eq, Show)

newtype Board = Board EOBoard-- | Board (SBoard)
  deriving (Eq, Show)

data EOBoard = EOBoard {foundations :: [Foundation], columns :: [Column], reserve :: Reserve}
  deriving (Eq)

instance Show EOBoard where
  show (EOBoard fs cs rs) = "Foundations: " ++ show fs ++ "\nColumns: " ++ show cs ++ "\nReserve: " ++ show rs

------------------------------------------------
toFoundations :: EOBoard -> EOBoard
toFoundations board = toFoundations' board EOBoard {foundations = foundations board, columns = columns board, reserve = reserve board}
  where
      --                temp       save to    output
    toFoundations' :: EOBoard -> EOBoard -> EOBoard
    toFoundations' board@(EOBoard fs cs rs) board'@(EOBoard fs' cs' rs')
        












