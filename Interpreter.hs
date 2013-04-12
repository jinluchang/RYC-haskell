module Main where

import Control.Monad
import Data.Array

import System.Environment

import Parser
import Evaluation

main :: IO ()
main = do
    args <- getArgs
    str <- getContents
    let prog = readProg str
        melody = variablePadding (envGen prog ! 0)
    when ("-v" `elem` args) $ do
        putStrLn str
        putStrLn $ showDefns prog
    putStrLn $ "song = " ++ show melody
