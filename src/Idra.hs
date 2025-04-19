{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A composable, monadic and ergonomic EDSL for text RPG games in Haskell
module Idra (
  MonadIdra
, Game
, Action
, Options
, Message
, Input
, IdraException
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

import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base64 (encode, decode)
import Control.Monad (ap)
import Data.Functor (void)
import GHC.Utils.Exception (IOException, tryIO)
import Control.Monad.Trans (lift)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Data.List (intercalate)
import Text.Read (readMaybe)
import Test.QuickCheck (Gen, generate)

newtype Idra a = Idra {unIdra :: IO (Maybe a)}
-- | The monad of the module. Represents a game with game state of type s
-- and produces a value of type a. It is a member of MonadState s.
-- It is recommended that you use a record type for s where you store
-- global information about your game such as the player's inventory,
-- health and gold. It is not recommended that you store location or
-- scene states in s. Use calls to other Game values for this.
newtype Game s a = Game (StateT s Idra a) deriving (Functor, Applicative, Monad, MonadState s)
-- | An Action is a message describing the action a player takes
-- and the resulting Game action.
data Action s a = Action {getMessage :: Message, getGame :: Game s a}
-- | An Option is a list of possible Actions a player can choose from.
type Options s a = [Action s a]

-- | A Message is a String that the game displays to the player. It exists to make type declarations more informative in terms of intention.
type Message = String
-- | An Input is a String that the user entered into the game. It exists to make type declarations more informative in terms of intention.
type Input = String
-- | IdraException is a type produced by loadGame. It can either communicate
-- a parse failure of the loaded savefile or an IOException arising
-- from trying to access the file.
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

-- | MonadIdra provides the service of ending the game. It is not defined exclusively for Game since one may want to use it in a monad transformer.
class MonadIdra f where
  -- | Ends the game as soon as this action is reached, skips all following actions. WARNING: Should permanently hang if one has sequenced an infinite number of actions after, but no sensible reason to do this has been found.
  endGame :: f ()

instance MonadIdra Idra where
  endGame = Idra $ return Nothing

instance MonadIdra (Game s) where
  endGame = idraToGame endGame

inRange :: (Ord a) => a -> a -> a -> Bool
inRange b v t = b <= v && v <= t

-- Base 64 encoding functions for strings
encode' :: String -> String
encode' = unpack . encode . pack

decode' :: String -> Either String String
decode' s = fmap unpack $ decode $ pack s

nats :: [Int]
nats = [1,2..]

-- Lifting Idra to Game
idraToGame :: Idra a -> Game s a
idraToGame = Game . lift

-- Lifting IO to Idra
liftIdra :: IO a -> Idra a
liftIdra ma = Idra $ Just <$> ma

-- | Lifting IO to Game
liftGame :: IO a -> Game s a
liftGame = idraToGame . liftIdra

-- | Prints a message
message :: Message -> Game s ()
message m = liftGame $ putStrLn m

-- | Prints a system message, not "in narrative", but gives information to the player.
-- A Message "Test!" is displayed as "-- Test! --".
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

-- | Creates an action
action :: Message -> Game s a -> Action s a
action m g = Action {getMessage=m, getGame=g}

-- | Turns Options into a Game by showing the options
-- and asking the user for an answer until they respond with a valid answer
options :: Options s a -> Game s a
options opts = let n = length opts in do
  printOptions opts
  raw <- readChoice
  case raw of
    Nothing -> invalidOptionInt >> options opts
    Just v -> if inRange 1 v n then getGame (opts !! (v-1)) else invalidOptionRange n >> options opts

-- | Read the user's input
input :: Game s String
input = liftGame getLine

-- | Takes a helper function that communicates what is wrong with
-- the input message (if something is wrong).
-- If the input message is valid, the helper function should return the empty string.
-- The function validInput interactively asks the user for input
-- and provides feedback until the error message from the helper function is empty.
-- It then returns the user's valid answer.
validInput :: (Input -> Message) -> Game s String
validInput helper = do
  raw <- input
  let errorMsg = helper raw
  case errorMsg of
    ""  -> return raw
    err -> systemMessage err >> validInput helper

-- | The game state s and extra string is saved
-- The extra string can be used to identify position in the game that was saved or additional information not included in the game state.
-- Returning Right () means a successful save, otherwise a Left IOException is returned.
-- Either is used instead of Maybe for consistency with loadGame. The base 64 conversion is not meant to be a safe encryption, but simply not let the savefile be in plaintext for a player to read.
saveGame :: Show s => FilePath -> String -> Game s (Either IOException ())
saveGame path info = do
  gameState <- get
  let save = (info,gameState)
  output <- liftGame $ tryIO $ writeFile path $ encode' $ show save
  case output of
    Left err -> return $ Left err
    Right () -> return $ Right ()

-- | Loads a game. Game state is updated to the loaded game state and additional String information is returned if the read was successful.
-- If the load fails through a file error, an IOException as an IdraException will be returned.
-- If the load fails through parsing the file content, the IdraException ParseFailure will be returned.
loadGame :: Read s => FilePath -> Game s (Either IdraException String)
loadGame path = do
  res <- liftGame $ tryIO $ readFile path :: Game s (Either IOException String)
  case res of
    Left err  -> return $ Left $ IOExc err
    Right str -> case decode' str of
      Left _        -> return $ Left ParseFailure
      Right decoded -> case readMaybe decoded of
        Nothing               -> return $ Left ParseFailure
        Just (info,gameState) -> do
          put gameState
          return $ Right info

-- | Lift a QuickCheck generator to Game. Uses QuickCheck's generate in its definition.
genGame :: Gen a -> Game s a
genGame g = liftGame $ generate g

-- | Run a Game with a starting game state. This is how you actually play your game!
runGame :: s -> Game s () -> IO ()
runGame s (Game g) = void m
  where Idra m = evalStateT g s

