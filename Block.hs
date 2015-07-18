module Block (Field,
              Row,
              Block (..),
              inverseRows,
              inverseCols,
              flipLeft,
              flipRight,
              getBlock,
              pretty,
              prettys
             ) where

import Control.Monad
import Data.List

type Field = [Row]
type Row   = [Int]

data Block =
    I |
    J |
    L |
    O |
    S |
    T |
    Z  deriving (Show, Read, Eq, Ord)

getBlock :: Block -> Field
getBlock I = [[0,0,0,0],
              [1,1,1,1],
              [0,0,0,0],
              [0,0,0,0]]

getBlock J = [[1,0,0],
              [1,1,1],
              [0,0,0]]

getBlock L = [[0,0,1],
              [1,1,1],
              [0,0,0]]

getBlock O = [[1,1],
              [1,1]]

getBlock S = [[0,1,1],
              [1,1,0],
              [0,0,0]]

getBlock T = [[0,1,0],
              [1,1,1],
              [0,0,0]]

getBlock Z = [[1,1,0],
              [0,1,1],
              [0,0,0]]

inverseRows :: Field -> Field
inverseRows = map reverse

inverseCols :: Field -> Field
inverseCols x = transpose $ map reverse (transpose x)

flipLeft :: Block -> Field -> Field
flipLeft I f = transpose f
flipLeft S f = transpose f
flipLeft Z f = transpose f
flipLeft x f = (transpose . inverseRows) f

flipRight :: Block -> Field -> Field
flipRight I f = transpose f
flipRight S f = transpose f
flipRight Z f = transpose f
flipRight x f = (transpose . inverseCols) f

{-| Add block at x,y in field by addition -}
addAt :: Field -> (Int, Int) -> Field -> Field
addAt field (x, y) block = undefined

-- test/debug

pretty :: Field -> IO ()
pretty f = forM_ f $ \row -> putStrLn (show row)

prettys :: [Field] -> IO ()
prettys fs = forM_ fs $ \f -> pretty f >> putStrLn "----"

leftTest :: Block -> [Field]
leftTest b = flipTest b flipLeft

rightTest :: Block -> [Field]
rightTest b = flipTest b flipRight

flipTest :: Block -> (Block -> Field -> Field) -> [Field]
flipTest b f = [first, second, third, fourth, fifth]
    where first  = getBlock b
          second = f b first
          third  = f b second
          fourth = f b third
          fifth  = f b fourth

testField :: Field
testField = [[0,0,0,1,1,1,1,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0,0]]
