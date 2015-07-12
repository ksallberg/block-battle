import Control.Monad
import System.IO

test :: String -> String
test inp | head (words inp) == "action" = "left,left,down,right"
         | otherwise = "no_moves"

loop :: IO ()
loop =
    do line <- getLine
       putStrLn $ test line
       hFlush stdout
       eof  <- isEOF
       unless eof loop

main =
    do hSetBuffering stdin LineBuffering
       loop
