module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import System.IO

{-| A Context is a StateT wrapper around the game's state,
    allowing usage of the inner IO moand.
-}
type Context = StateT GameState IO

data GameState = GameState {
    playerName :: String,
    board      :: [Int]
}

-- crude debugging flag
debug' :: Bool
debug' = True

debug :: IO () -> Context ()
debug x = when debug' (liftIO x)

test :: String -> String
test inp | head (words inp) == "action" = "left,left,down,right"
         | otherwise = "no_moves"

modstep :: Context ()
modstep = do
    lastVal <- get
    put (lastVal{ playerName = "brunhilde" })
    return ()

loop :: Context ()
loop = do
    state <- get
    line  <- (liftIO getLine)
    liftIO (putStrLn $ test line)
    liftIO (hFlush stdout)
    eof <- liftIO isEOF
    debug (putStrLn (playerName state))
    modstep
    unless eof loop

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    evalStateT loop $ GameState{playerName = "gretchen",
                                board      = [10]
                               }
