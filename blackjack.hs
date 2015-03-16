import Debug.Trace
import Prelude hiding (lookup)
import Text.Read (readMaybe)
import Data.List (nub, intercalate)
import Data.Maybe (fromJust)
import Control.Monad (guard, when, unless)
import Data.Foldable (msum)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import System.Random
import Data.Array.IO

data Suit = Spades | Clubs | Hearts | Diamonds deriving (Show, Eq)
data Rank = Ace | King | Queen | Jack | Rank Int deriving Eq
                                                          
showRank :: Rank -> String
showRank Ace = "Ace"
showRank King = "King"
showRank Queen = "Queen"
showRank Jack = "Jack"
showRank (Rank x) = ["", "", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten"] !! x

instance Show Rank where
  show = showRank

data Action = Hit | Stand | Double | Split | Surrender deriving (Show, Eq)

data RoundOutcome = Win | Loss | Push deriving (Show, Eq)

data Game = Game { cards :: [Card],
                   totalScore :: Int,
                   doubledDown :: Bool } deriving Show

type GameS a = StateT Game a

card :: Int -> Card
card 10 = Card (Queen, Spades)
card 11 = Card (Ace, Spades)
card x = Card (Rank x, Spades)

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do randomPosition <- getStdRandom (randomR (0, length xs - 1))
                let (left, a:right) = splitAt randomPosition xs
                fmap (a:) (shuffle (left ++ right))

joinWith :: String -> String -> [String] -> String
joinWith _ _ [] = ""
joinWith _ _ [a] = a
joinWith _ second ([a,b]) = a ++ second ++ b
joinWith first second (a:xs) = a ++ first ++ joinWith first second xs

newtype Card = Card (Rank, Suit) deriving (Eq)
                                                 
instance Show Card where
  show (Card (r, s)) = show r ++ " of " ++ show s

type Hand = [Card]
type Box = (Hand, Int)

showHand :: Hand -> String
showHand = joinWith ", " " and " . map show 

findOutcome :: Hand -> Hand -> RoundOutcome
findOutcome d p
  | playerPoints > dealerPoints = Win
  | dealerPoints == playerPoints && playerPoints /= 0 = Push
  | otherwise = Loss
  where dealerPoints = maximumPoints d
        playerPoints = maximumPoints p
    
deck :: [Card]
deck = [ Card (r, s) | r <- [Ace, King, Queen, Jack] ++ map Rank [2..10], s <- [Spades, Clubs, Hearts, Diamonds]]

value :: Card -> [Int]
value (Card (Rank x, _)) = [x]
value (Card (Ace, _)) = [1, 11]
value (Card (_, _)) = [10]

possiblePoints :: Hand -> [Int]
possiblePoints hand = filter (<=21) . (0:) . nub $ map sum $ mapM value hand
 
maximumPoints :: Hand -> Int
maximumPoints hand = if maxP == 21 && length hand == 2 then 22 else maxP
  where maxP = (maximum . possiblePoints) hand

presentOptions :: [(a, String)] -> IO a
presentOptions xs = printPrompt >> fmap (fst . (xs!!) . subtract 1) (readNumberUntilValid (\n -> n > 0 && n <= length xs) "Didn't catch that, please try again.")
  where printPrompt = putStrLn ("Please choose: " ++ (intercalate ", " . map (\(i, (a, s)) -> s ++ " (" ++ show i ++ ")") . zip ([1..] :: [Int])) xs ++ ":")

readNumber :: (Int -> Bool) -> String -> MaybeT IO Int
readNumber p err = do v <- MaybeT (fmap readMaybe getLine)
                      guard (p v)
                      return v

readNumberUntilValid :: (Int -> Bool) -> String -> IO Int
readNumberUntilValid p err = (fmap fromJust . runMaybeT . msum) (readNumber p err : repeat ((liftIO . putStrLn) err >> readNumber p err))

gameLoop :: GameS IO ()
gameLoop = do bet <- inputBet
              playersBox <- drawToBox ([], bet)
              dealersBox <- drawToBox ([], 0)
              playersBox <- drawToBox playersBox
              let dealersCard = (head . fst) dealersBox
              playersBox <- resolveBox getPlayersAction dealersCard playersBox
              dealersBox <- resolveBox getDealersAction dealersCard dealersBox
              adjustScore playersBox (fst dealersBox)
              showState (fst dealersBox) (fst playersBox)
              shuffleIfNeeded
              gameLoop

isBlackJack :: Hand -> Bool
isBlackJack = (==22) . maximumPoints

adjustScore :: Box -> Hand -> GameS IO ()
adjustScore box dealersHand  = do state <- get
                                  let bet = snd box
                                  let currentScore = totalScore state
                                  let playersHand = fst box
                                  let threeHalves x = ceiling $ (1.5 :: Float) * fromIntegral x
                                  let adjustedScore = case dealersHand `findOutcome` playersHand of
                                                        Win -> currentScore + if isBlackJack playersHand then threeHalves bet else bet 
                                                        Loss -> currentScore - bet
                                                        Push -> currentScore
                                  put state { totalScore = adjustedScore }

inputBet :: GameS IO Int
inputBet = liftIO $ putStrLn "How much do you bet?" >> readNumberUntilValid (>0) "Didn't catch that, please try again"
 
shuffleIfNeeded :: GameS IO ()
shuffleIfNeeded = do state <- get
                     let leftPercentCards p state = (length . cards) state < ceiling (p * 52.0 * fromIntegral numberOfDecksInUse)
                     when (leftPercentCards 0.25 state) $ do shuffledShoe <- liftIO newShuffledShoe
                                                             put state { cards = shuffledShoe }
                                                             liftIO $ putStrLn "Shuffled"
 
showHandWithPoints :: Hand -> String
showHandWithPoints h = showHand h ++ " (" ++ (show . maximumPoints) h ++ ")"

showState :: Hand -> Hand -> GameS IO ()
showState dealersHand playersHand = do state <- get
                                       liftIO $ do putStrLn ("Dealer's hand: " ++ showHandWithPoints dealersHand)
                                                   putStrLn ("Your hand: " ++ showHandWithPoints playersHand)
                                                   putStrLn (case dealersHand `findOutcome` playersHand of
                                                                Push -> "Push"
                                                                Win -> "You win!"
                                                                Loss -> "You loose")
                                                   putStrLn ("Current score: " ++ show (totalScore state))
                                                   putStr "Press <Enter> to continue" >> getLine 
                                                   (putStrLn . concat . replicate 50) "-"

resolveBox :: (Card -> Hand -> IO Action) -> Card -> Box -> GameS IO Box
resolveBox getAction dealersCard b = if maximumPoints (fst b) `elem` [0, 21, 22] then return b
                                     else do action <- lift $ getAction dealersCard (fst b)
                                             state <- get
                                             case action of
                                               Stand -> return b
                                               Double -> drawToHand (fst b) >>= \h -> return (h, snd b * 2)
                                               Hit -> drawToBox b >>= resolveBox getAction dealersCard

getPlayersAction :: Card -> Hand -> IO Action
getPlayersAction c h = do putStrLn $ "Dealer's first card is " ++ show c
                          putStrLn $ "Your hand is: " ++ showHand h
                          presentOptions $ map (\a -> (a, show a)) ([Hit, Stand] ++ [Double | length h == 2])

getDealersAction :: Card -> Hand -> IO Action 
getDealersAction c h = return $ if maximumPoints h < 17 then Hit else Stand
 
numberOfDecksInUse :: Int
numberOfDecksInUse = 1

newShuffledShoe :: IO [Card]
newShuffledShoe = (shuffle . concat . replicate numberOfDecksInUse) deck

drawToHand :: Hand -> GameS IO Hand
drawToHand h = do state <- get
                  let topCard = (head . cards) state
                  put state { cards = tail (cards state) }
                  return (topCard:h)

drawToBox :: Box -> GameS IO Box
drawToBox b = drawToHand (fst b) >>= \h -> return (h, snd b)

main = do shuffledShoe <- newShuffledShoe
          evalStateT gameLoop Game { cards = shuffledShoe,
                                     totalScore = 0,
                                     doubledDown = False }
          
