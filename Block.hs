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
              fieldScore,
              splitBy
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

allRotations :: Block -> [(Field, Int)]
allRotations I = [(getBlock I, 0), (flipLeft I (getBlock I), 1)]
allRotations S = [(getBlock S, 0), (flipLeft S (getBlock S), 1)]
allRotations Z = [(getBlock Z, 0), (flipLeft Z (getBlock Z), 1)]
allRotations x = [(first, 0), (second, 1), (third, 2), (fourth, 3)]
    where first  = getBlock x
          second = flipLeft x first
          third  = flipLeft x second
          fourth = flipLeft x third

clearField :: Field -> Field
clearField f = [[changeRule r | r <- row] | row <- f]
    where changeRule 1 = 0
          changeRule 3 = 2 -- don't use 3 as black row, keepOK messes up
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
addAt content (x, 0) (row:rows) =
    case x >= (length row) || x < 0 of
        True  -> (row:rows) -- not possible, skip
        False -> (before ++ [cell + content] ++ after) : rows
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
fieldScore f =
    weighted - (emptyInRows + emptyInCols)
    where fIndex      = (zip f (map (*10) [1..])) :: [([Int], Int)]
          emptyInRows = sum $ map numberWords f
          emptyInCols = sum $ map numberWords (transpose f)
          weighted    = sum rowValues
          rowValues   = map (\(row, weight) -> sum (map (*weight) row)) fIndex

{-| Length of the longest word of non zeros -}
completeRow :: [Int] -> Bool
completeRow = all (==1)

numberWords :: [Int] -> Int
numberWords row = case (length (filter (/=[]) (splitBy 0 row))) of
                      1 -> 0
                      x -> x

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy delimiter = foldr f [[]]
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs

parseField :: String -> Field
parseField str = let fieldParts = splitBy ';' str
                 in [map (\x -> read x :: Int) (splitBy ',' fieldPart)
                     | fieldPart <- fieldParts]

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
             [0,0,0,1,1,0,0,0,0,0],
             [0,0,1,1,1,0,0,0,0,0],
             [0,0,1,1,1,1,0,0,0,0],
             [1,1,1,1,1,1,1,1,1,1],
             [1,1,1,1,1,1,1,1,1,1]]

testField1 :: Field
testField1 = [[0,0,0,0,0,0,0,0,0,0],
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
             [0,0,0,1,1,0,0,0,0,0],
             [0,0,1,1,1,0,0,0,0,0],
             [0,0,1,1,1,1,0,0,1,0],
             [0,1,1,1,1,1,0,0,1,1],
             [1,1,1,1,1,1,1,1,0,1]]

testField2 :: Field
testField2 = [[0,0,0,0,0,0,0,0,0,0],
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
             [0,0,0,1,1,0,0,0,0,0],
             [0,0,1,1,1,0,0,0,0,0],
             [0,0,1,1,1,1,0,1,0,0],
             [0,1,1,1,1,1,1,1,1,0],
             [1,1,1,1,1,1,1,1,1,0]]

testParse :: Field
testParse = parseField $ "0,0,0,0,0,0,0,0,0,0;0,0,0,0,0,0,0,0,0,0;" ++
                         "0,0,0,0,0,0,0,0,0,0;0,0,0,0,0,0,0,0,0,0;" ++
                         "0,0,0,0,0,0,0,0,0,0;0,0,0,2,0,0,0,0,0,0;" ++
                         "2,2,2,2,0,2,2,0,2,0;2,2,2,0,2,2,2,2,2,0;" ++
                         "0,0,2,0,2,2,2,2,0,0;2,2,2,2,2,2,2,0,2,0;" ++
                         "0,2,2,0,0,2,0,2,2,0;0,2,2,2,2,2,2,2,2,0;" ++
                         "2,2,2,0,2,2,0,0,2,0;2,2,0,2,0,2,2,0,2,0;" ++
                         "2,2,0,2,2,2,2,0,2,0;2,2,2,2,2,2,2,0,2,0;" ++
                         "0,0,2,0,0,2,2,0,2,0;2,2,2,0,2,2,0,2,2,0;" ++
                         "3,3,3,3,3,3,3,3,3,0;3,3,3,3,3,3,3,3,3,0"
