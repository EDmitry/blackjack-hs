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
import Data.Map (Map, lookup, adjust)
import qualified Data.Map as Map (fromList)

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

data PlayerType = Dealer | Player deriving (Show, Eq, Ord)

data RoundOutcome = Win | Loss | Push deriving (Show, Eq)

data Game = Game { cards :: [Card],
                   hands :: Map PlayerType Hand,
                   totalScore :: Int,
                   doubledDown :: Bool
                 } deriving Show

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

newtype Hand = Hand { fromHand :: [Card] }

instance Show Hand where
  show = joinWith ", " " and " . map show . fromHand

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
possiblePoints hand = filter (<=21) . (0:) . nub $ map sum $ mapM value (fromHand hand)
 
maximumPoints :: Hand -> Int
maximumPoints hand = if maxP == 21 && length (fromHand hand) == 2 then 22 else maxP
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

resetRound :: GameS IO ()
resetRound = do state <- get
                put state { hands = Map.fromList [(Dealer, Hand []), (Player, Hand [])],
                            doubledDown = False }

gameLoop :: GameS IO ()
gameLoop = do resetRound
              bet <- inputBet
              drawToPlayerType Player
              drawToPlayerType Dealer
              drawToPlayerType Player
              resolveForPlayerType Player
              resolveForPlayerType Dealer
              adjustScore bet
              showState
              shuffleIfNeeded
              gameLoop

isBlackJack :: Hand -> Bool
isBlackJack = (==22) . maximumPoints

adjustScore :: Int -> GameS IO ()
adjustScore bet = do state <- get
                     let finalBet = if doubledDown state then bet * 2 else bet
                     let currentScore = totalScore state
                     let playersHand = handForType Player state
                     let threeHalves x = ceiling $ 1.5 * fromIntegral x
                     let adjustedScore = case handForType Dealer state `findOutcome` playersHand of
                                           Win -> currentScore + if isBlackJack playersHand then threeHalves finalBet else finalBet
                                           Loss -> currentScore - finalBet
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
showHandWithPoints h = show h ++ " (" ++ (show . maximumPoints) h ++ ")"

showState :: GameS IO ()
showState = do state <- get
               liftIO $ do putStrLn ("Dealer's hand: " ++ showHandWithPoints (handForType Dealer state))
                           putStrLn ("Your hand: " ++ showHandWithPoints (handForType Player state))
                           putStrLn (case handForType Dealer state `findOutcome` handForType Player state of
                                       Push -> "Push"
                                       Win -> "You win!"
                                       Loss -> "You loose")
                           putStrLn ("Current score: " ++ show (totalScore state))
                           putStr "Press <Enter> to continue" >> getLine 
                           (putStrLn . concat . replicate 50) "-"

handForType :: PlayerType -> Game -> Hand
handForType pt = fromJust . lookup pt . hands

resolveForPlayerType :: PlayerType -> GameS IO ()
resolveForPlayerType pt = do maximumPoints <- fmap (maximumPoints . handForType pt) get
                             unless (maximumPoints `elem` [0, 21, 22]) $ do
                                state <- get
                                let dealersCard = (head . fromHand . handForType Dealer) state
                                action <- lift $ getActionForPlayerType pt dealersCard (handForType pt state)
                                case action of
                                    Stand -> return ()
                                    Double -> put state { doubledDown = True } >> drawToPlayerType pt >> return ()
                                    Hit -> drawToPlayerType pt >> resolveForPlayerType pt

getActionForPlayerType :: PlayerType -> Card -> Hand -> IO Action
getActionForPlayerType Player = getPlayersAction
getActionForPlayerType Dealer = getDealersAction
                               
getPlayersAction :: Card -> Hand -> IO Action
getPlayersAction c h = do putStrLn $ "Dealer's first card is " ++ show c
                          putStrLn $ "Your hand is: " ++ show h
                          presentOptions $ map (\a -> (a, show a)) ([Hit, Stand] ++ [Double | (length . fromHand) h == 2])

getDealersAction :: Card -> Hand -> IO Action 
getDealersAction c h = return $ if maximumPoints h < 17 then Hit else Stand
 
addCardToHand :: Card -> Hand -> Hand
addCardToHand c h = Hand $ c:fromHand h

numberOfDecksInUse :: Int
numberOfDecksInUse = 1

newShuffledShoe :: IO [Card]
newShuffledShoe = (shuffle . concat . replicate numberOfDecksInUse) deck

drawToPlayerType :: PlayerType -> GameS IO Card
drawToPlayerType pt = do state <- get
                         let topCard = (head . cards) state
                         put state { cards = tail (cards state),
                                     hands = adjust (addCardToHand topCard) pt (hands state)}
                         return topCard
       
main = do shuffledShoe <- newShuffledShoe
          evalStateT gameLoop Game { cards = shuffledShoe,
                                     hands = Map.fromList [(Dealer, Hand []), (Player, Hand [])],
                                     totalScore = 0,
                                     doubledDown = False }
          
