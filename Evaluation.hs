module Evaluation where

import Data.List
import Data.Array
import Data.Maybe
import Data.Char
import Control.DeepSeq

import System.Environment
import System.IO.Unsafe
import Debug.Trace

import Parser

fi :: (Integral a, Num b) => a -> b
fi x = fromIntegral x

compile :: [Name] -> [Name] -> Expr -> ExprB
compile envG = go where
    go env expr = case expr of
        Boo b -> BooB b
        Num n -> NumB n
        Chr c -> ChrB c
        Note e1 e2 -> NoteB (go env e1) (go env e2)
        App e1 e2 -> AppB (go env e1) (go env e2)
        Seq es -> SeqB $ map (go env) es
        Par es -> ParB $ map (go env) es
        Let ds e -> LetB (map (go env' . snd) ds) (go env' e)
            where env' = map fst ds ++ env
        Lam x body  -> LamB $ go (x:env) body
        Var x -> case findIndex (==x) env of
            Just n -> BoundB n
            Nothing -> case findIndex (==x) envG of
                Just n -> BoundGB n
                Nothing -> VarB x

eval :: ExprB -> EnvG -> Env -> ExprC
eval expr envG = go expr where
    go e env = case e of
        VarB x -> VarC x
        BooB b -> BooC b
        NumB n -> NumC n
        ChrB c -> ChrC c
        SeqB es -> SeqC $ map (\eb -> go eb env) es
        ParB es -> ParC $ map (\eb -> go eb env) es
        NoteB e1 e2 -> NoteC (go e1 env) (go e2 env)
        AppB e1 e2 -> apply (go e1 env) (go e2 env)
        LetB ds e' -> go e' env'
            where env' = map (\eb -> go eb env') ds ++ env
        BoundB n -> env !! n
        BoundGB n -> envG ! n
        LamB body -> LamC $ \vx -> go body (vx:env)
    apply (LamC f) arg = f arg
    apply fun arg = AppC fun arg

envGen :: [Defn] -> EnvG
envGen defns = envG where
    (fns, fs) = unzip $ ("song", fromJust $ lookup "song" defns) : filter ((/= "song") . fst) defns
    envG = listArray (0, length defns + length primitives - 1) $
        (map (\eb -> eval eb envG []) $ map (compile (fns ++ map fst primitives) []) fs) ++ map snd primitives

variablePadding :: ExprC -> Expr
variablePadding = go names where
    go ns expr = case expr of
        VarC x -> Var x
        BooC b -> Boo b
        NumC n -> Num n
        ChrC c -> Chr c
        NoteC e1 e2 -> Note (go ns e1) (go ns e2)
        AppC e1 e2 -> App (go ns e1) (go ns e2)
        SeqC es -> Seq $ map (go ns) es
        ParC es -> Par $ map (go ns) es
        LamC f -> Lam (head ns) $ go (tail ns) $ f (VarC $ head ns)
    names = map ("$var"++) $ map show ([0..] :: [Int])

-- ------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------

primitives :: [(Name, ExprC)]
primitives =
    [ ("^"          , LamC $ mapMelody $ raise 12)
    , ("_"          , LamC $ mapMelody $ raise (-12))
    , ("#"          , LamC $ mapMelody $ raise 1)
    , ("&"          , LamC $ mapMelody $ raise (-1))
    , ("/"          , LamC $ \n1 -> LamC $ \n2 -> NumC $ getNumC n1 / getNumC n2)
    , ("*"          , LamC $ \n1 -> LamC $ \n2 -> NumC $ getNumC n1 * getNumC n2)
    , ("+"          , LamC $ \n1 -> LamC $ \n2 -> NumC $ getNumC n1 + getNumC n2)
    , ("-"          , LamC $ \n1 -> LamC $ \n2 -> NumC $ getNumC n1 - getNumC n2)
    , (">"          , LamC $ \n1 -> LamC $ \n2 -> BooC $ getNumC n1 > getNumC n2)
    , ("<"          , LamC $ \n1 -> LamC $ \n2 -> BooC $ getNumC n1 < getNumC n2)
    , (">="         , LamC $ \n1 -> LamC $ \n2 -> BooC $ getNumC n1 >= getNumC n2)
    , ("<="         , LamC $ \n1 -> LamC $ \n2 -> BooC $ getNumC n1 <= getNumC n2)
    , ("=="         , LamC $ \e1 -> LamC $ \e2 -> BooC $ e1 == e2)
    , ("/="         , LamC $ \e1 -> LamC $ \e2 -> BooC $ e1 /= e2)
    , ("not"        , LamC $ \(BooC b) -> BooC $ not b)
    , ("or"         , LamC $ \b1 -> LamC $ \b2 -> BooC $ getBooC b1 || getBooC b2)
    , ("and"        , LamC $ \b1 -> LamC $ \b2 -> BooC $ getBooC b1 && getBooC b2)
    , ("chr"        , LamC $ \n -> ChrC . chr . round $ getNumC n)
    , ("ord"        , LamC $ \c -> NumC . fi . ord $ getChrC c)
    , ("getArgs"    , getArgsC)
    , ("readFile"   , LamC $ readFileC)
    , ("char?"      , LamC $ BooC . isChr)
    , ("nil?"       , LamC $ BooC . isNil)
    , ("par?"       , LamC $ BooC . isPar)
    , ("seq?"       , LamC $ BooC . isSeq)
    , ("car"        , LamC $ car)
    , ("cdr"        , LamC $ cdr)
    , ("par"        , LamC $ \x -> LamC $ \parxs -> parL x parxs)
    , ("seq"        , LamC $ \x -> LamC $ \seqxs -> seqL x seqxs)
    , ("time"       , LamC $ \e -> NumC $ getTime e)
    , ("pitch"      , LamC $ \e -> NumC $ getPitch e)
    , ("if"         , LamC $ \b -> LamC $ \e1 -> LamC $ \e2 -> if getBooC b then e1 else e2)
    , ("true"       , BooC True)
    , ("false"      , BooC False)
    , ("show"       , LamC $ \e -> SeqC . map ChrC . showExprC $ e )
    , ("read"       , LamC $ \e -> convertC . readExpr . map getChrC $ getSeqC e)
    , ("force"      , LamC $ \e -> deepseq e $ LamC id)
    , ("trace"      , LamC $ \e -> trace (map getChrC . getSeqC $ e) $ LamC id) ]

convertC :: Expr -> ExprC
convertC (Boo b) = BooC b
convertC (Num n) = NumC n
convertC (Chr n) = ChrC n
convertC (Note e1 e2) = NoteC (convertC e1) (convertC e2)
convertC (Seq es) = SeqC $ map convertC es
convertC (Par es) = ParC $ map convertC es
convertC e = error $ "convertC : can not convert : " ++ showExpr e

parL :: ExprC -> ExprC -> ExprC
parL x parxs = ParC $ x:xs where
    xs = case parxs of
        ParC ys -> ys
        e -> error $ "par : second argument is not par list : " ++ showExprC e

seqL :: ExprC -> ExprC -> ExprC
seqL x seqxs = SeqC $ x:xs where
    xs = case seqxs of
        SeqC ys -> ys
        e -> error $ "seq : second argument is not par list : " ++ showExprC e

car :: ExprC -> ExprC
car (ParC (x:_)) = x
car (SeqC (x:_)) = x
car e = error $ "car : list is empty or not a list : " ++ showExprC e

cdr :: ExprC -> ExprC
cdr (ParC (_:xs)) = ParC xs
cdr (SeqC (_:xs)) = SeqC xs
cdr e = error $ "cdr : list is empty or not a list : " ++ showExprC e

getArgsC :: ExprC
getArgsC = SeqC . map (SeqC . map ChrC) . unsafePerformIO $ getArgs

readFileC :: ExprC -> ExprC
readFileC (SeqC cs) | all isC cs = SeqC . map ChrC . unsafePerformIO . readFile $ map getChrC cs where
    isC (ChrC _) = True
    isC _ = False
readFileC e = error $ "readFileC : not a filename : " ++ showExprC e

getChrC :: ExprC -> Char
getChrC (ChrC c) = c
getChrC e = error $ "getChrC : " ++ showExprC e

getBooC :: ExprC -> Bool
getBooC (BooC b) = b
getBooC e = error $ "getBooC : " ++ showExprC e

getSeqC :: ExprC -> [ExprC]
getSeqC (SeqC es) = es
getSeqC e = error $ "getSeqC : " ++ showExprC e

getParC :: ExprC -> [ExprC]
getParC (ParC es) = es
getParC e = error $ "getParC : " ++ showExprC e

getNumC :: ExprC -> Number
getNumC (NumC n) = n
getNumC e = error $ "getNumC : " ++ showExprC e

isChr :: ExprC -> Bool
isChr (ChrC _) = True
isChr _ = False

isNil :: ExprC -> Bool
isNil (ParC []) = True
isNil (SeqC []) = True
isNil _ = False

isPar :: ExprC -> Bool
isPar (ParC []) = True
isPar _ = False

isSeq :: ExprC -> Bool
isSeq (SeqC []) = True
isSeq _ = False

raise :: Int -> ExprC -> ExprC
raise k = mapMelody go where
    go (NoteC (NumC n) t) = NoteC (NumC (loop k n)) t
    go (NumC n) = NumC (loop k n)
    go ec = error $ "change pitch is not allowed for this argument: " ++ showExpr (variablePadding ec)
    loop n | n > 0 = r . loop (n-1)
           | n < 0 = l . loop (n+1)
           | otherwise = id
    r 0 = 0
    r 1.0 = 1.5
    r 1.5 = 2.0
    r 2.0 = 2.5
    r 2.5 = 3.0
    r 3.0 = 4.0
    r 4.0 = 4.5
    r 4.5 = 5.0
    r 5.0 = 5.5
    r 5.5 = 6.0
    r 6.0 = 6.5
    r 6.5 = 7.0
    r 7.0 = 9.0
    r n | n >= 8 = r (n-8) + 8
        | n < 0  = r (n+8) - 8
    r n = error $ "Not a note " ++ show n
    l 0 = 0
    l 1.0 = -1.0
    l 1.5 = 1.0
    l 2.0 = 1.5
    l 2.5 = 2.0
    l 3.0 = 2.5
    l 4.0 = 3.0
    l 4.5 = 4.0
    l 5.0 = 4.5
    l 5.5 = 5.0
    l 6.0 = 5.5
    l 6.5 = 6.0
    l 7.0 = 6.5
    l n | n >= 8 = l (n-8) + 8
        | n < 0 = l (n+8) - 8
    l n = error $ "Not a note " ++ show n

getTime :: ExprC -> Number
getTime (SeqC es) = sum $ map getTime es
getTime (ParC es) = maximum $ map getTime es
getTime (NumC _) = 1
getTime (NoteC _ (NumC t)) = t
getTime e = error $ "time : not a melody : " ++ showExprC e

getPitch :: ExprC -> Number
getPitch (NumC n) = n
getPitch (NoteC (NumC n) _) = n
getPitch e = error $ "pitch : not a note : " ++ showExprC e

mapMelody :: (ExprC -> ExprC) -> ExprC -> ExprC
mapMelody f (SeqC es) = SeqC $ map (mapMelody f) es
mapMelody f (ParC es) = ParC $ map (mapMelody f) es
mapMelody f e = f e

showExprC :: ExprC -> String
showExprC = showExpr . variablePadding
