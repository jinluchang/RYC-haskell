module Main where

import Data.List
import Data.Function
import Data.Array
import Control.Monad

import System.Environment

import Codec.Midi

import Parser
import Evaluation

main :: IO ()
main = do
    args <- getArgs
    str <- if "-v" /= args !! 0
        then readFile $ args !! 0
        else readFile $ args !! 1
    let prog = readProg str
        melody = variablePadding $ envGen prog ! 0
        track = deltaList . eventList . fillDefault $ melody
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
        putStrLn ""
        putStrLn "-----------------------------------------------------------------------------------------"
        putStrLn "Sequence --------------------------------------------------------------------------------"
        putStrLn ""
        mapM_ print $ track
    exportFile "a.mid" $ Midi
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
