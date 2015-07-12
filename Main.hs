module Main where

import System.IO

main :: IO ()
main =
    do done <- isEOF
       case done of
           True -> return ()
           False -> do
               x <- getLine
               case (head (words x)) of
                   "settings" ->
                       main
                   "update" ->
                       main
                   "action" ->
                       putStrLn "drop" >>
                       main
                   _ ->
                       return ()
