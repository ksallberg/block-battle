module Block (Field,
              Row,
              Pair,
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
              seekBottom,
              avoidEmptys,
              splitBy,
              getCell,
              getCoordsOfField,
              isGrounded,
              allPositions,
              oneToZero,
              oneToZeroNoBlack,
              usedFieldHeight
             ) where

import Control.Monad
import Data.List

type Field = [Row]
type Row   = [Int]
type Pair  = (Int, Int)

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
flipRight I f = transpose f
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

oneToZero :: Int -> Int
oneToZero 1 = 0
oneToZero x = x

oneToZeroNoBlack :: Int -> Int
oneToZeroNoBlack 1 = 0
oneToZeroNoBlack 3 = 2
oneToZeroNoBlack x = x

clearField :: (Int -> Int) -> Field -> Field
clearField changeRule f = [[changeRule r | r <- row] | row <- f]

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
                       | (cell, xCount) <- zip row [0..]]))
          (0, [])
          field

avoidEmptys :: Field -> Int
avoidEmptys f =
    weighted `div` (max 1 emptyInRows * max 1 emptyInCols)
    where fIndex      = (zip f (map (*1000) [1..])) :: [([Int], Int)]
          emptyInRows = sum $ map numberWords f
          emptyInCols = sum $ map numberWords (transpose f)
          weighted    = sum rowValues
          rowValues   = map (\(row, weight) -> sum (map (*weight) row)) fIndex

seekBottom :: Field -> Int
seekBottom f =
    weighted - 400 * (max 1 emptyInRows * max 1 emptyInCols)
    where fIndex      = (zip f (map (*1000) [1..])) :: [([Int], Int)]
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

getCell :: Pair -> Field -> Maybe Int
getCell (x, y) f | y >= length f        = Nothing
                 | x >= length (head f) = Nothing
                 | otherwise            = Just $ (f !! y) !! x

getCoordsOfField :: Field -> [Pair]
getCoordsOfField f = concat
    [[(x, y) | x <- [0..(length row)-1], row !! x == 1]
     | (y, row)  <- zip [0..] f]

--cutBottom :: Field -> Field
--cutBottom f = reverse $ dropWhile (\row -> sum row == 0) (reverse f)

{-
    Pick out the bottom line (max y) of the coordinates
    of a tetris block (representes as a Field), and then
    look at the positions below these on the game field.
    The positions below should not all be (Just 0) because
    then the block is hanging in the air.
-}
isGrounded :: (Pair, Field, Field, Int) -> Bool
isGrounded ((xpos, ypos), f, rot, _amount) =
    let coords    = [(x + xpos, y + ypos + 1)
                     | (x, y) <- getCoordsOfField rot]
        allX      = nub (map fst coords)
        lowCoords = [(x, maximum (map snd $ filter (\(cx,_) -> cx == x) coords))
                    | x <- allX]
        hasGround = [getCell p f | p <- lowCoords]
 --       blHeight  = length $ cutBottom rot
    in not $ all (==Just 0) hasGround

--isGrounded' :: (Pair, Field, Field, Int) -> [Maybe Int]
isGrounded' ((xpos, ypos), f, rot, _amount) =
    let coords    = [(x + xpos, y + ypos + 1)
                     | (x, y) <- getCoordsOfField rot]
        allX      = nub (map fst coords)
        lowCoords = [(x, maximum (map snd $ filter (\(cx,_) -> cx == x) coords))
                     | x <- allX]
        hasGround = [getCell p f | p <- lowCoords]
--        blHeight  = length $ cutBottom rot
    in lowCoords

allPositions :: Int -> Int -> Field -> [Pair]
allPositions fieldWidth fieldHeight f = positions
    where topSpace    = length $ takeWhile match f
          bottomSpace = length $ takeWhile match (reverse f)
          leftSpace   = length $ takeWhile match (transpose f)
          rightSpace  = length $ takeWhile match ((reverse . transpose) f)
          blockWidth  = length (head f)
          blockHeight = length f
          minX        = - leftSpace
          maxX        = fieldWidth  - blockWidth  + rightSpace
          minY        = - topSpace
          maxY        = fieldHeight - blockHeight + bottomSpace
          positions   = [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]
          match       = \row -> sum row == 0

usedFieldHeight :: Field -> Int
usedFieldHeight f = length noEmpty
     where noBlack = takeWhile (\row -> not $ all (==3) row) f
           noEmpty = dropWhile (\row -> sum row == 0) noBlack

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

testIsGrounded :: Bool
testIsGrounded = isGrounded ((1,14), testField, getBlock O, 0)

--testIsGrounded' :: [Maybe Int]
testIsGrounded' = isGrounded' ((1,14), testField, getBlock O, 0)


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
