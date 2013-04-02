module Main where

import Control.Monad

import System.Environment

import Parser
import Evaluation

main :: IO ()
main = do
    args <- getArgs
    let sourceFilename = head $ filter ((/='-') . head) args
    str <- readFile sourceFilename
    let prog = readProg str
        melody = eval (envGen prog) [] (Var "song")
    when ("-v" `elem` args) $ do
        putStrLn str
        putStrLn $ showDefns prog
    putStrLn $ "song = " ++ show melody
