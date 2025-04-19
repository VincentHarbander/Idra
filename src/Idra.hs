{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Idra (
  Game
, Action
, Options
, Message
, Input
, endGame
, liftGame
, message
, systemMessage
, action
, options
, input
, validInput
, saveGame
, loadGame
, genGame
, runGame
) where

import Control.Monad (ap)
import Data.Functor (void)
import GHC.Utils.Exception (IOException, tryIO)
import Control.Monad.Trans (lift)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Data.List (intercalate)
import Text.Read (readMaybe)
import Test.QuickCheck (Gen, generate)

newtype Idra a = Idra {unIdra :: IO (Maybe a)}
newtype Game s a = Game (StateT s Idra a) deriving (Functor, Applicative, Monad, MonadState s)
data Action s a = Action {getMessage :: Message, getGame :: Game s a}
type Options s a = [Action s a]

type Message = String
type Input = String
data IdraException = ParseFailure | IOExc IOException

instance Functor Idra where
  fmap f (Idra ma) = Idra (fmap f <$> ma)

instance Applicative Idra where
  pure x = Idra $ return $ Just x
  (<*>) = ap

instance Monad Idra where
  return = pure
  (Idra ma) >>= f = Idra $ do
    maybea <- ma
    case maybea of
      Nothing -> return Nothing
      Just a  -> unIdra $ f a

class MonadIdra f where
  endGame :: f ()

instance MonadIdra Idra where
  endGame = Idra $ return Nothing

instance MonadIdra (Game s) where
  endGame = idraToGame endGame

inRange :: (Ord a) => a -> a -> a -> Bool
inRange b v t = b <= v && v <= t

nats :: [Int]
nats = [1,2..]

-- Lifting Idra to Game
idraToGame :: Idra a -> Game s a
idraToGame = Game . lift

-- Lifting IO to Idra
liftIdra :: IO a -> Idra a
liftIdra ma = Idra $ Just <$> ma

-- Lifting IO to Game
liftGame :: IO a -> Game s a
liftGame = idraToGame . liftIdra

-- Prints message
message :: Message -> Game s ()
message m = liftGame $ putStrLn m

-- Prints system message, not in narrative
systemMessage :: Message -> Game s ()
systemMessage m = message $ "-- " ++ m ++ " --"

-- Print that option should have been an integer
invalidOptionInt :: Game s ()
invalidOptionInt = systemMessage "The input should be an integer"

-- Print invalid option range message
invalidOptionRange :: Int -> Game s ()
invalidOptionRange n = systemMessage $ "Options can only be between 1 and " ++ show n

-- Pretty print the user's available options
printOptions :: Options s a -> Game s ()
printOptions opts = message optionText
  where optionText = intercalate "\n" optionList
        optionList = zipWith (++) numbering li
        li = getMessage <$> opts
        numbering = (++ ". ") . show <$> nats

-- Try to read the user's choice
readChoice :: Game s (Maybe Int)
readChoice = liftGame $ do
  raw <- getLine
  return $ readMaybe raw

-- Create an action
action :: Message -> Game s a -> Action s a
action m g = Action {getMessage=m, getGame=g}

-- Turn options into a Game by showing the options and asking the user for an answer until they respond with a valid answer
options :: Options s a -> Game s a
options opts = let n = length opts in do
  printOptions opts
  raw <- readChoice
  case raw of
    Nothing -> invalidOptionInt >> options opts
    Just v -> if inRange 1 v n then getGame (opts !! (v-1)) else invalidOptionRange n >> options opts

-- Read the user's input
input :: Game s String
input = liftGame getLine

-- Takes a helper function that communicates what is wrong with the input message if it is.
-- If the input message is valid, it will return the empty string
-- The function interactively asks the user for input and provides feedback until the error message is empty
-- It then returns the user's valid answer
validInput :: (Input -> Message) -> Game s String
validInput helper = do
  raw <- input
  let errorMsg = helper raw
  case errorMsg of
    ""  -> return raw
    err -> systemMessage err >> validInput helper

-- The GameState s and extra string is saved
-- The extra string can be used to identify position in the game that was saved or additional information not included in the GameState
-- Returning Right () means a successful save, otherwise a Left IOException is returned
-- Either is used instead of Maybe for consistency with loadGame
saveGame :: Show s => FilePath -> String -> Game s (Either IOException ())
saveGame path info = do
  gameState <- get
  let save = (info,gameState)
  output <- liftGame $ tryIO $ writeFile path (show save)
  case output of
    Left err -> return $ Left err
    Right () -> return $ Right ()

-- Loads a game. GameState is updated according to the load Game and additional String information is returned if the read was successful
-- If the load fails through a file error, an IOException as an IdraException will be returned
-- If the load fails through parsing the file content, the IdraException ParseFailure will be returned
loadGame :: Read s => FilePath -> Game s (Either IdraException String)
loadGame path = do
  res <- liftGame $ tryIO $ readFile path :: Game s (Either IOException String)
  case res of
    Left err  -> return $ Left $ IOExc err
    Right str -> case readMaybe str of
      Nothing               -> return $ Left ParseFailure
      Just (info,gameState) -> do
        put gameState
        return $ Right info

-- Use a QuickCheck generator
genGame :: Gen a -> Game s a
genGame g = liftGame $ generate g

-- Run Game with a starting game state
runGame :: s -> Game s () -> IO ()
runGame s (Game g) = void m
  where Idra m = evalStateT g s

