module Main where

import Control.Monad
import Data.Array

import System.Console.GetOpt
import System.Environment
import System.Directory
import System.FilePath

import Parser
import Evaluation

data Flag
    = Verbose
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
    [ Option "v"   ["verbose"]    (NoArg Verbose) "show many intermediate information" ]

compileOpts :: [String] -> ([Flag], [String])
compileOpts argv = case getOpt Permute options argv of
    (opts, args, []) -> (opts, args)
    e -> error $ show e

main :: IO ()
main = do
    (flags, args) <- liftM compileOpts $ getArgs
    strs <- mapM readFile args
    setCurrentDirectory $ takeDirectory . head $ args
    let prog = concat $ map readProg strs
        melody = variablePadding $ envGen prog ! 0
    when (Verbose `elem` flags) $ do
        putStrLn "-----------------------------------------------------------------------------------------"
        putStrLn "Original Program ------------------------------------------------------------------------"
        putStrLn ""
        mapM_ putStrLn strs
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
