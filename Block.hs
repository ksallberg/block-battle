module Block (Field,
              Row,
              Block (..),
              inverseRows,
              inverseCols,
              insertBlock,
              flipLeft,
              flipRight,
              getBlock,
              allRotations,
              clearField,
              pretty,
              prettys,
              testField,
              fieldScore
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
flipLeft x f = (transpose . inverseRows) f

flipRight :: Block -> Field -> Field
flipRight I f = flipLeft I f
flipRight x f = (transpose . inverseCols) f

allRotations :: Block -> [Field]
allRotations I = [getBlock I, flipLeft I (getBlock I)]
allRotations S = [getBlock S, flipLeft S (getBlock S)]
allRotations Z = [getBlock Z, flipLeft Z (getBlock Z)]
allRotations x = [first, second, third, fourth]
    where first  = getBlock x
          second = flipLeft x first
          third  = flipLeft x second
          fourth = flipLeft x third

clearField :: Field -> Field
clearField f = [[changeRule r | r <- row] | row <- f]
    where changeRule 1 = 0
          changeRule x = x

{-| Insert a block starting at a coordinate, in a field. Results in
    a new field where the block exists. Plus operation is used to merge
    the block and field. -}
insertBlock :: Field -> (Int, Int) -> Field -> Field
insertBlock block (x, y) field =
    foldl (\field' (coord, val) -> addAt val coord field')
          field
          (changeInstructions block (x, y))

{-| Add block at x,y in field by addition. -}
addAt :: Int -> (Int, Int) -> Field -> Field
addAt _ _ [] = []
addAt content (x, 0) (row:rows) = case (drop x row) of
    [] -> error $ "problem" ++ (show row) ++ ", x: " ++ (show x)
    _  -> (before ++ [cell + content] ++ after) : rows
    where before = take x row
          cell   = head $ drop x row
          after  = tail $ drop x row
addAt content (x, y) (row:rows) = row : (addAt content (x, y - 1) rows)

{-| The fold operation here results in the type (Int, [((Int, Int), Int)])
    from that we keep only the list of instructions, this list
    is a coordinate and a value. This value can be inserted at a coordinate
    in a field. -}
changeInstructions :: Field -> (Int, Int) -> [((Int, Int), Int)]
changeInstructions field (x, y) = snd $
    foldl (\(yCount, acc) row ->
              (yCount + 1,
               acc ++ [((x + xCount, y + yCount), cell)
                          | (cell, xCount) <- zip row [0..]])
          )
          (0, [])
          field

fieldScore :: Field -> Int
fieldScore f = sum $ map (\(ls,w) -> sum ls * w) (zip f (map (*100) [1..]))

-- test/debug

pretty :: Field -> IO ()
pretty f = forM_ f $ \row -> putStrLn $ show row

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

testInsertBlock :: (Int, Int) -> IO ()
testInsertBlock coord = pretty $ insertBlock (getBlock T) coord testField

testField :: Field
testField = [[0,0,0,0,0,0,0,0,0,0],
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
             [0,0,0,0,0,0,2,2,2,2]]
