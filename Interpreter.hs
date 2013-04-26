module Main where

import Control.Monad

import System.Console.GetOpt
import System.Environment

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
    (flags, args) <- liftM (compileOpts . takeWhile (/="---")) $ getArgs
    if args == [] then error "No input files" else return ()
    strs <- mapM readFile args
    let prog = concat $ map readProg strs
        melody = variablePadding $ interpret (envGen prog) (Var "song")
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
