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

type HandResolution = (Hand, Bool, Maybe Hand)

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

equalValue :: Hand -> Bool
equalValue [Card (ra, _), Card (rb, _)] = ra == rb
equalValue _ = False
               
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

isBlackJack :: Hand -> Bool
isBlackJack = (==22) . maximumPoints

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

showState :: [Box] -> Hand -> GameS IO ()
showState playersBoxes dealersHand = do state <- get
                                        liftIO $ putStrLn ("Dealer's hand: " ++ showHandWithPoints dealersHand)
                                        mapM_ (showStateForBox dealersHand . fst) playersBoxes
                                        liftIO $ do putStrLn ("Current score: " ++ show (totalScore state))
                                                    putStr "Press <Enter> to continue" >> getLine 
                                                    (putStrLn . concat . replicate 50) "-"

showStateForBox :: Hand -> Hand -> GameS IO ()
showStateForBox dealersHand playersHand = do state <- get
                                             liftIO $ do let result = case dealersHand `findOutcome` playersHand of
                                                                        Push -> "Push"
                                                                        Win -> "Win!"
                                                                        Loss -> "Loss"
                                                         putStrLn ("Your hand: " ++ showHandWithPoints playersHand ++ ": " ++ result)

gameLoop :: GameS IO ()
gameLoop = do bet <- inputBet
              playersBox <- drawToBox ([], bet)
              dealersBox@(dealersCard:xs, _) <- drawToBox ([], 0)
              playersBox <- drawToBox playersBox
              playersBoxes <- resolveBox getPlayersAction dealersCard playersBox
              ((dealersHand, _):ds) <- resolveBox getDealersAction dealersCard dealersBox
              adjustScore playersBoxes dealersHand
              showState playersBoxes dealersHand 
              shuffleIfNeeded
              gameLoop

adjustScore :: [Box] -> Hand -> GameS IO ()
adjustScore playersBoxes dealersHand  = mapM_ (adjustScoreForBox dealersHand) playersBoxes

adjustScoreForBox :: Hand -> Box -> GameS IO ()
adjustScoreForBox dealersHand (playersHand, bet) = do state <- get
                                                      let currentScore = totalScore state
                                                      let threeHalves x = ceiling $ (1.5 :: Float) * fromIntegral x
                                                      let adjustedScore = case dealersHand `findOutcome` playersHand of
                                                                            Win -> currentScore + if isBlackJack playersHand then threeHalves bet else bet 
                                                                            Loss -> currentScore - bet
                                                                            Push -> currentScore
                                                      put state { totalScore = adjustedScore }

resolveBox :: (Card -> Hand -> IO Action) -> Card -> Box -> GameS IO [Box]
resolveBox f c (h, bet) = do (playersHand, playerDoubled, splittedHand) <- resolveHand f c (h, False, Nothing)
                             let resultingBox = (playersHand, if playerDoubled then bet * 2 else bet)
                             rest <- resolveMaybeBox f c (splittedHand, bet)
                             return (resultingBox:rest)
  where resolveMaybeBox f c (Nothing, _) = return []
        resolveMaybeBox f c (Just h, bet) = resolveBox f c (h, bet)

resolveHand :: (Card -> Hand -> IO Action) -> Card -> HandResolution -> GameS IO HandResolution
resolveHand getAction dealersCard hr@(h, d, sh) = if maximumPoints h `elem` [0, 21, 22] then return hr 
                                                  else do action <- lift $ getAction dealersCard h
                                                          state <- get
                                                          case action of
                                                            Stand -> return hr
                                                            Double -> drawToHand h >>= \h' -> return (h', True, sh)
                                                            Split -> do h' <- drawToHand (tail h)
                                                                        newHand <- drawToHand [head h]
                                                                        return (h', d, Just newHand) 
                                                                        resolveHand getAction dealersCard (h', d, Just newHand)
                                                            Hit -> drawToHandResoltion hr >>= resolveHand getAction dealersCard

getPlayersAction :: Card -> Hand -> IO Action
getPlayersAction c h = do putStrLn $ "Dealer's first card is " ++ show c
                          putStrLn $ "Your hand is: " ++ showHand h
                          presentOptions $ map (\a -> (a, show a)) ([Hit, Stand] ++ [Double | length h == 2] ++ [Split | equalValue h])

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

drawToHandResoltion :: HandResolution -> GameS IO HandResolution
drawToHandResoltion (h, d, sc) = drawToHand h >>= \h' -> return (h', d, sc)

drawToBox :: Box -> GameS IO Box
drawToBox (h, bet) = drawToHand h >>= \h' -> return (h', bet)

main = do shuffledShoe <- newShuffledShoe
          evalStateT gameLoop Game { cards = shuffledShoe,
                                     totalScore = 0,
                                     doubledDown = False }
          
