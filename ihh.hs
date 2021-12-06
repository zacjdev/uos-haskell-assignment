data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Eq, Show, Ord, Enum, Bounded)

data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Eq, Show, Ord, Enum, Bounded)

newtype Card = Card (Suit, Pip)
  deriving (Eq, Show)

type Foundations = [Deck]

type Columns = [Deck]

type Reserves = Deck

type Deck = [Card]

type SColumns = [SDeck]

type SFoundations = [SDeck]

type Stock = SDeck

data SCard = SCard (Suit, Pip, Bool)

type SDeck = [SCard]

data SBoard = SBoard (Stock , SColumns, SFoundations)

data EOBoard = EOBoard (Foundations, Columns, Reserves)

-- board can be an SBoard or an EOBoard
data Board = Board2 SBoard | Board EOBoard

divide :: Integer -> Integer -> Double
divide x y = fromIntegral x / fromIntegral y

analyseEO :: Int -> Int -> (Int, Int)
analyseEO seed games = (wins, winrate)
  where
    wins = games
    winrate = seed