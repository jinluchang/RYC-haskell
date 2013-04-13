module Main where

import Control.Monad
import Data.Array

import System.Environment

import Parser
import Evaluation

main :: IO ()
main = do
    args <- getArgs
    str <- if "-v" /= args !! 0
        then readFile $ args !! 0
        else readFile $ args !! 1
    let prog = readProg str
        melody = variablePadding (envGen prog ! 0)
    when ("-v" == args !! 0) $ do
        putStrLn "-----------------------------------------------------------------------------------------"
        putStrLn "Original Program ------------------------------------------------------------------------"
        putStrLn ""
        putStrLn str
        putStrLn ""
        putStrLn "-----------------------------------------------------------------------------------------"
        putStrLn "Parsed Program --------------------------------------------------------------------------"
        putStrLn ""
        putStrLn $ showDefns prog
        putStrLn ""
        putStrLn "-----------------------------------------------------------------------------------------"
        putStrLn "Result ----------------------------------------------------------------------------------"
        putStrLn ""
    putStrLn $ "song = " ++ show melody
