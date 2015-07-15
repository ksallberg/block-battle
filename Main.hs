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
    playerName :: String,
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
          | head (words str) == "update"   = handleUpdate   (tail $ words str)
          | head (words str) == "settings" = handleSettings (tail $ words str)

{-| Handle the action given by the admin script!
    Make use of already set game state. -}
handleAction :: String -> Context()
handleAction str = do
    liftIO $ putStrLn "left,left,down,right"

{-| Update the game state with configurations received
    with the update  flag from the admin script -}
-- TODO: implement complete parsing of admin commands
handleUpdate :: [String] -> Context ()
handleUpdate ["game", "round", round] = do
    state <- get
    return ()
handleUpdate ["game", "this_piece_type", piece] = do
    state <- get
    return ()
handleUpdate ["game", "next_piece_type", piece] = do
    state <- get
    return ()
handleUpdate ["game", "this_piece_position", pos]   = do
    state <- get
    return ()
handleUpdate [playername, "row_points"] = do
    state <- get
    return ()
handleUpdate [playername, "combo"] = do
    state <- get
    return ()
handleUpdate [playername, "field"] = do
    state <- get
    return ()
handleUpdate _ = error "Unsupported update received!"

-- TODO: implement complete parsing of admin commands
handleSettings :: [String] -> Context ()
handleSettings ["timebank", time] = do
    debug (putStrLn $ "Set timebank to: " ++ time)
    state <- get
    put $ state{ timebank = (read time :: Int) }
handleSettings ["time_per_move", time] = do
    state <- get
    return ()
handleSettings ["player_names", names] = do
    state <- get
    return ()
handleSettings ["your_bot", botname] = do
    state <- get
    return ()
handleSettings ["field_width", width] = do
    state <- get
    return ()
handleSettings ["field_height", height] = do
    state <- get
    return ()
handleSettings _ = error "Unsupported setting received!"

loop :: Context ()
loop = do
    line  <- (liftIO getLine)
    parse line
    state <- get
    debug $ putStrLn $ "timebank is: " ++ (show $ timebank state)
    liftIO (hFlush stdout)
    eof <- liftIO isEOF
    unless eof loop

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    evalStateT loop $ GameState{timebank=0}
