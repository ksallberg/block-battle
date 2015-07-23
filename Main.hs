{-
Copyright (c) 2015, Kristian Sällberg <kristian@purestyle.se>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

Neither the name of the author nor the names of its contributors
may be used to endorse or promote products derived from this software
without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
-}
module Main where

import Block
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Data.List
import Data.Ord
import System.IO
import System.Random

{-| A Context is a StateT wrapper around the game's state,
    allowing usage of the inner IO moand.
-}
type Context = StateT GameState IO

data Move =
    StepDown  |
    StepLeft  |
    StepRight |
    TurnLeft  |
    TurnRight |
    Drop      |
    NoMoves deriving (Eq, Ord)

instance Show Move where
    show StepDown  = "down"
    show StepLeft  = "left"
    show StepRight = "right"
    show TurnLeft  = "turnleft"
    show TurnRight = "turnright"
    show Drop      = "drop"
    show NoMoves   = "no_moves"

data Player = Player {
    playerName :: String,
    rowPoints  :: Int,
    combo      :: Int,
    field      :: Field
} deriving Show

data GameState = GameState {
    timebank          :: Int,
    timePerMove       :: Int,
    players           :: [Player],
    myBot             :: String,
    fieldHeight       :: Int,
    fieldWidth        :: Int,
    gameRound         :: Int,
    thisPieceType     :: Block,
    nextPieceType     :: Block,
    thisPiecePosition :: (Int, Int)
} deriving Show

-- crude debugging flag
debug' :: Bool
debug' = False

debug :: IO () -> Context ()
debug x = when debug' (liftIO x)

p2p :: (Int, Int) -> (Int, Int) -> [Move]
p2p (x1, y1) (x2, y2) = xPath ++ [StepDown | _ <- [1..yDelta]]
    where xDelta = x2 - x1
          xPath  = case xDelta > 0 of
                            True ->  [StepRight | _ <- [1..xDelta]]
                            False -> [StepLeft  | _ <- [1..(abs xDelta)]]
          yDelta = abs $ y2 - y1

pathToEmpty :: Field -> (Int, Int) -> Block -> [Move]
pathToEmpty f (x, y) b = p2p (x, y) (5,18)
    where investigate = reverse $ take 2 (reverse f)

getHeight :: Block -> Int
getHeight I = 4
getHeight O = 2
getHeight _ = 3

{-
    Right now this is the AI functionality, given a block and a field,
    try to find the best possible location of the block...
-}
allPositions :: Int -> Int -> Block -> [(Int, Int)]
allPositions fieldHeight fieldWidth b = positions
    where height    = getHeight b
          maxX      = fieldWidth - height
          maxY      = fieldHeight - height
          positions = [(x, y) | x <- [0..(maxX)], y <- [0..(maxY)]]

insertBlockToField :: Block -> Field -> [(Int, Int)] -> [((Int,Int), Field, Int)]
insertBlockToField b f posLs =
    [(pos, insertBlock rot pos f, count) | pos <- posLs, (count, rot) <- blockRotations]
    where blockRotations = zip [0..] (allRotations b)

optimizePath :: [Move] -> [Move]
optimizePath xs = case xs /= diff of
                      True  -> diff ++ [Drop]
                      False -> xs
    where diff = reverse $ dropWhile (== StepDown) (reverse xs)

keepOK :: [((Int, Int), Field, Int)] -> [((Int, Int), Field, Int)]
keepOK fs = filter rule fs
    where rule (_, f, _) = not $ elem 3 (concat f)

merge' :: [[a]] -> [a]
merge' = concat . transpose

merge :: [a] -> [a] -> [a]
merge l r = merge' [l,r]

{-| Handle the action given by the admin script!
    Make use of already set game state. -}
handleAction :: Int -> Context ()
handleAction moves = do
    state    <- get
    myPlayer <- getMyBot -- type Player
    gen      <- liftIO getStdGen
    let myField   = clearField (field myPlayer)
        block     = thisPieceType state
        positions = allPositions (fieldHeight state)
                                 (fieldWidth  state)
                                 block
        allFields = keepOK $ insertBlockToField block myField positions
        weighted  = sortBy (comparing fst) [(fieldScore f, x)
                                            | x@(pos, f, _count) <- allFields]
        weightedS = map snd weighted
        (finalPos, _, count) = last weightedS
        movePlan = merge [TurnLeft | _ <- [1..count]]
                         (p2p (thisPiecePosition state) finalPos)
    --error (show aaa)
--    liftIO $ putStrLn $ "hej: " ++ (show $ last $ keepOK $ aaa)
    liftIO $ putStrLn $ formatMoves (optimizePath movePlan)

-------------
-- PARSING --
-------------

parse :: [String] -> Context ()
parse ["action", "moves", move] = handleAction (read move :: Int)
parse ("update":xs)             = parseUpdate xs
parse ("settings":xs)           = parseSettings xs
parse _                         = error "Unsupported command!"

parseSettings :: [String] -> Context ()
parseSettings ["timebank", time] = do
    debug $ putStrLn $ "Set timebank to: " ++ time
    state <- get
    put $ state{ timebank = (read time :: Int) }
parseSettings ["time_per_move", time] = do
    state <- get
    put $ state{ timePerMove = (read time :: Int) }
parseSettings ["player_names", names] = do
    state <- get
    let namesLs   = splitBy ',' names
        playersLs = foldl (\acc name -> acc ++ [Player{playerName = name,
                                                       rowPoints  = 0,
                                                       combo      = 0,
                                                       field      = []
                                                      }])
                          []
                          namesLs
    put $ state{ players = playersLs }
parseSettings ["your_bot", botname] = do
    state <- get
    put $ state{ myBot = botname }
parseSettings ["field_width", width] = do
    state <- get
    put $ state{ fieldWidth = (read width :: Int) }
parseSettings ["field_height", height] = do
    state <- get
    put $ state{ fieldHeight = (read height :: Int) }
parseSettings _ = error "Unsupported setting received!"

{-| Update the game state with configurations received
    with the update  flag from the admin script -}
parseUpdate :: [String] -> Context ()
parseUpdate ["game", "round", roundVal] = do
    state <- get
    put $ state{ gameRound = (read roundVal :: Int) }
parseUpdate ["game", "this_piece_type", piece] = do
    state <- get
    put $ state{ thisPieceType = (read piece :: Block) }
parseUpdate ["game", "next_piece_type", piece] = do
    state <- get
    put $ state{ nextPieceType = (read piece :: Block) }
parseUpdate ["game", "this_piece_position", pos] = do
    state <- get
    let [x, y] = splitBy ',' pos
    put $ state{ thisPiecePosition = (read x :: Int, read y :: Int) }
parseUpdate [playern, "row_points", rowPointsVal] = do
    state <- get
    let updatePls = [case playerName pl == playern of
                         True  -> pl{rowPoints = read rowPointsVal :: Int}
                         False -> pl
                     | pl <- players state]
    put $ state{ players = updatePls }
parseUpdate [playern, "combo", comboVal] = do
    state <- get
    let updatePls = [case playerName pl == playern of
                         True  -> pl{combo = read comboVal :: Int}
                         False -> pl
                     | pl <- players state]
    put $ state{ players = updatePls }
parseUpdate [playern, "field", fieldVal] = do
    state <- get
    let fieldParts = splitBy ';' fieldVal
        fieldLs    = [map (\x -> read x :: Int) (splitBy ',' fieldPart)
                      | fieldPart <- fieldParts]
        updatePls = [case playerName pl == playern of
                        True  -> pl{field = fieldLs}
                        False -> pl
                     | pl <- players state]
    put $ state{ players = updatePls }
parseUpdate _ = error "Unsupported update received!"

loop :: Context ()
loop = do
    line  <- liftIO getLine
    parse (words line)
    state <- get
    debug $ putStrLn $ "GameState: " ++ show state
    liftIO (hFlush stdout)
    eof   <- liftIO isEOF
    unless eof loop

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    evalStateT loop $ GameState{timebank          = 0,
                                timePerMove       = 0,
                                players           = [],
                                myBot             = "not set",
                                fieldHeight       = 20,
                                fieldWidth        = 10,
                                gameRound         = 0,
                                thisPieceType     = I,
                                nextPieceType     = O,
                                thisPiecePosition = (3, -1)}

-- helper functions
getMyBot :: Context Player
getMyBot = do
    st <- get
    case [pl | pl <- players st, playerName pl == myBot st] of
        []    -> error "Bot not found!"
        [bot] -> return $ bot

formatMoves :: [Move] -> String
formatMoves xs = tail $ foldl (\acc next -> acc ++ "," ++ show next) "" xs

splitBy :: Char -> String -> [String]
splitBy delimiter = foldr f [[]]
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs
