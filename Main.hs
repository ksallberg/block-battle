module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import System.IO

{-| A Context is a StateT wrapper around the game's state,
    allowing usage of the inner IO moand.
-}
type Context = StateT GameState IO

data Block =
    I |
    J |
    L |
    O |
    S |
    T |
    Z  deriving (Show, Read, Eq, Ord)

data Player = Player {
    playerName :: Int,
    rowPoints  :: Int,
    combo      :: Int,
    field      :: [[Int]]
}

data GameState = GameState {
    timebank          :: Int,
    timePerMove       :: Int,
    players           :: [Player],
    myBot             :: Player,
    fieldHeight       :: Int,
    fieldWidth        :: Int,
    round             :: Int,
    thisPieceType     :: Block,
    nextPieceType     :: Block,
    thisPiecePosition :: (Int, Int)
}

-- crude debugging flag
debug' :: Bool
debug' = False

debug :: IO () -> Context ()
debug x = when debug' (liftIO x)

getBlock :: Block -> [[Int]]
getBlock I = [[0,0,0,0], [1,1,1,1], [0,0,0,0], [0,0,0,0]]
getBlock J = [[1,0,0], [1,1,1], [0,0,0]]
getBlock L = [[0,0,1], [1,1,1], [0,0,0]]
getBlock O = take 2 $ repeat [1,1]
getBlock S = [[0,1,1], [1,1,0], [0,0,0]]
getBlock T = [[0,1,0], [1,1,1], [0,0,0]]
getBlock Z = [[1,1,0], [0,1,1], [0,0,0]]

modstep :: Context ()
modstep = do
    lastVal <- get
    put $ lastVal{ timebank = 10000 }
    return ()

parse :: String -> Context ()
parse str | head (words str) == "action"   = handleAction   str
          | head (words str) == "update"   = handleUpdate   str
          | head (words str) == "settings" = handleSettings str

{-| Handle the action given by the admin script!
    Make use of already set game state.
-}
handleAction :: String -> Context()
handleAction str = do
    liftIO $ putStrLn "left,left,down,right"

handleSettings :: String -> Context ()
handleSettings str = return ()

handleUpdate :: String -> Context ()
handleUpdate str = return ()

loop :: Context ()
loop = do
    state <- get
    line  <- (liftIO getLine)
    parse line
    liftIO (hFlush stdout)
    eof <- liftIO isEOF
    debug $ putStrLn $ show $ timebank state
    modstep
    unless eof loop

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    evalStateT loop $ GameState{}
