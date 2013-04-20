module Main where

import Data.List
import Data.Function
import Data.Maybe
import Control.Monad

import System.Console.GetOpt
import System.Environment
import System.Directory
import System.FilePath

import Codec.Midi

import Parser
import Evaluation

data Flag
    = Verbose
    | OutputFilename String
  deriving (Eq, Show)

getOutputFilename :: Flag -> Maybe String
getOutputFilename (OutputFilename fn) = Just fn
getOutputFilename _ = Nothing

options :: [OptDescr Flag]
options =
    [ Option "v"   ["verbose"]    (NoArg Verbose) "show many intermediate information"
    , Option "o"   ["output"]     (ReqArg OutputFilename "output-filename")
        "output filename, default is based on input filename" ]

compileOpts :: [String] -> ([Flag], [String])
compileOpts argv = case getOpt Permute options argv of
    (opts, args, []) -> (opts, args)
    e -> error $ show e

main :: IO ()
main = do
    (flags, args) <- liftM (compileOpts . takeWhile (/="---")) $ getArgs
    if args == [] then error "No input files" else return ()
    strs <- mapM readFile args
    cwd <- getCurrentDirectory
    setCurrentDirectory $ takeDirectory . head $ args
    let prog = concat $ map readProg strs
        melody = variablePadding $ interpret (envGen prog) (Var "song")
        track = deltaList . eventList . fillDefault $ melody
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
        putStrLn ""
        putStrLn "-----------------------------------------------------------------------------------------"
        putStrLn "Sequence --------------------------------------------------------------------------------"
        putStrLn ""
        mapM_ print $ track
    let defaultOutputFileName = replaceExtension (takeFileName (head args)) ".mid"
        outputFileName = last . (defaultOutputFileName :) $ mapMaybe getOutputFilename flags
    exportFile (cwd </> outputFileName) $ Midi
        { fileType = SingleTrack
        , timeDiv = TicksPerBeat 230
        , tracks = [track] }

deltaList :: [(Int, Message)] -> [(Int, Message)]
deltaList = delta 0 . sortBy (compare `on` fst) where
    delta _    [] = [(500, TrackEnd)]
    delta l ((tx,x):rs) = (tx-l,x) : delta tx rs

eventList :: Expr -> [(Int, Message)]
eventList melody = go 0 melody where
    go start n@(Note _ _) = case getKey n of
        Nothing -> []
        Just k -> [ (start, NoteOn {channel = 1, key = k, velocity = 127})
                  , (start + getDuration n - 1, NoteOff {channel = 1, key = k, velocity = 127})]
    go start (Par es) = concatMap (go start) es
    go _     (Seq []) = []
    go start (Seq (e:es)) = go start e ++ go (start + getDuration e) (Seq es)
    go _ e = error $ "Final result is not a melody : " ++ showExpr e

getKey :: Expr -> Maybe Int
getKey (Note (Num x) _) = liftM (43+) $ go x
  where
    go 0 = Nothing
    go 1.0 = Just 0
    go 1.5 = Just 1
    go 2.0 = Just 2
    go 2.5 = Just 3
    go 3.0 = Just 4
    go 4.0 = Just 5
    go 4.5 = Just 6
    go 5.0 = Just 7
    go 5.5 = Just 8
    go 6.0 = Just 9
    go 6.5 = Just 10
    go 7.0 = Just 11
    go n | n >= 8 = liftM (+12) $ go (n-8)
         | n < 0 = liftM (subtract 12) $ go (n+8)
    go _ = error "pitch is not valid"
getKey _ = error "Not a note"

getDuration :: Expr -> Int
getDuration (Note _ (Num t)) = round $ 120 * t
getDuration (Seq es) = sum $ map getDuration es
getDuration (Par es) = maximum $ map getDuration es
getDuration _ = error "Not a note"

fillDefault :: Expr -> Expr
fillDefault expr = case expr of
    Num n -> Note (Num n) (Num 1)
    Seq es -> Seq $ map fillDefault es
    Par es -> Par $ map fillDefault es
    _ -> expr
