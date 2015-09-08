module Tests where

import Block
import Main
import Control.Monad
import Control.Monad.Trans.State.Lazy

strangeT = clearField oneToZero $ parseField $
           "0,0,0,1,1,1,0,0,0,0;0,0,0,0,0,0,0,0,0,0;0,0,0,0,0,0,0,0,0,0;" ++
           "0,0,0,0,0,0,0,0,0,0;0,0,0,0,0,0,0,0,0,0;0,0,0,0,0,0,0,0,0,0;" ++
           "0,0,0,0,0,0,0,0,0,0;0,0,0,0,0,0,0,0,0,0;0,0,0,0,0,0,0,0,0,0;" ++
           "0,0,0,0,0,0,0,0,0,0;0,0,0,0,0,0,0,0,0,0;0,0,0,0,0,0,0,0,0,0;" ++
           "0,0,0,0,0,2,2,0,0,0;0,0,0,2,0,2,2,2,2,2;0,0,2,2,0,2,2,2,2,2;" ++
           "0,2,2,2,2,2,2,2,2,2;0,2,2,2,2,2,2,2,2,2;0,2,2,2,2,2,2,2,2,2;" ++
           "3,3,3,3,3,3,3,3,3,3;3,3,3,3,3,3,3,3,3,3"

run :: IO ()
run = do
    let field = blogki --strangeT
        p = Player{playerName = "testo",
                   rowPoints  = 0,
                   combo      = 0,
                   field      = field}
        startX = handleAction 1000
    (f, pos) <- evalStateT startX $ GameState{timebank          = 0,
                                              timePerMove       = 0,
                                              players           = [p],
                                              myBot             = "testo",
                                              fieldHeight       = 20,
                                              fieldWidth        = 10,
                                              gameRound         = 0,
                                              thisPieceType     = L,
                                              nextPieceType     = S,
                                              thisPiecePosition = (3, -1)}
    putStrLn $ "Chosen coord: " ++ (show pos)
    pretty (insertBlock f pos field)

leftTest :: Block -> [Field]
leftTest b = flipTest b flipLeft

-- rightTest :: Block -> [Field]
-- rightTest b = flipTest b flipRight

flipTest :: Block -> (Block -> Field -> Field) -> [Field]
flipTest b f = [first, second, third, fourth, fifth]
    where first  = getBlock b
          second = f b first
          third  = f b second
          fourth = f b third
          fifth  = f b fourth

testIsGrounded :: Bool
testIsGrounded = isGrounded ((1,14), testField, getBlock O, 0)

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
             [0,0,0,2,2,0,0,0,0,0],
             [0,0,2,2,2,0,0,0,0,0],
             [0,0,2,2,2,2,0,0,0,0],
             [2,2,2,2,2,2,2,2,2,2],
             [2,2,2,2,2,2,2,2,2,2]]

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
              [0,0,0,2,2,0,0,0,0,0],
              [0,0,2,2,2,0,0,0,0,0],
              [0,0,2,2,0,2,0,0,2,0],
              [0,2,2,2,2,2,0,0,2,2],
              [2,2,2,2,2,2,2,2,2,2]]

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
              [0,0,0,2,2,0,0,0,0,0],
              [0,0,2,2,2,0,0,0,0,0],
              [0,0,2,2,2,2,0,2,0,0],
              [0,2,2,2,2,2,2,2,2,0],
              [2,2,2,2,2,2,2,2,2,0]]

testField3 :: Field
testField3 = [[0,0,0,0,0,0,0,0,0,0],
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
              [0,0,0,0,0,0,2,2,0,0],
              [0,0,0,0,0,2,2,2,2,0],
              [0,0,0,0,0,0,0,0,2,2]]



testParse :: Field
testParse = parseField $
    "0,0,0,0,0,0,0,0,0,0;0,0,0,0,0,0,0,0,0,0;0,0,0,0,0,0,0,0,0,0;" ++
    "0,0,0,0,0,0,0,0,0,0;0,0,0,0,0,0,0,0,0,0;0,0,0,2,0,0,0,0,0,0;" ++
    "2,2,2,2,0,2,2,0,2,0;2,2,2,0,2,2,2,2,2,0;0,0,2,0,2,2,2,2,0,0;" ++
    "2,2,2,2,2,2,2,0,2,0;0,2,2,0,0,2,0,2,2,0;0,2,2,2,2,2,2,2,2,0;" ++
    "2,2,2,0,2,2,0,0,2,0;2,2,0,2,0,2,2,0,2,0;2,2,0,2,2,2,2,0,2,0;" ++
    "2,2,2,2,2,2,2,0,2,0;0,0,2,0,0,2,2,0,2,0;2,2,2,0,2,2,0,2,2,0;" ++
    "3,3,3,3,3,3,3,3,3,3;3,3,3,3,3,3,3,3,3,3"

blogki :: Field
blogki =
   [[0,0,0,0,0,0,0,0,0,0],
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
    [0,0,2,0,0,0,0,0,0,0],
    [0,0,2,2,2,2,0,0,0,0],
    [2,0,2,2,2,2,2,2,2,0],
    [2,2,2,2,2,2,2,0,2,2]]
