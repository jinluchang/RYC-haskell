module Evaluation where

import Data.List
import Data.Array
import Data.Maybe
import Data.Char

import System.Environment
import System.IO.Unsafe

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
    [ ("^"        , LamC $ mapMelody $ raise 12)
    , ("_"        , LamC $ mapMelody $ raise (-12))
    , ("#"        , LamC $ mapMelody $ raise 1)
    , ("&"        , LamC $ mapMelody $ raise (-1))
    , ("/"        , LamC $ \(NumC n1) -> LamC $ \(NumC n2) -> NumC $ n1 / n2)
    , ("*"        , LamC $ \(NumC n1) -> LamC $ \(NumC n2) -> NumC $ n1 * n2)
    , ("+"        , LamC $ \(NumC n1) -> LamC $ \(NumC n2) -> NumC $ n1 + n2)
    , ("-"        , LamC $ \(NumC n1) -> LamC $ \(NumC n2) -> NumC $ n1 - n2)
    , (">"        , LamC $ \(NumC n1) -> LamC $ \(NumC n2) -> BooC $ n1 > n2)
    , ("<"        , LamC $ \(NumC n1) -> LamC $ \(NumC n2) -> BooC $ n1 < n2)
    , (">="       , LamC $ \(NumC n1) -> LamC $ \(NumC n2) -> BooC $ n1 >= n2)
    , ("<="       , LamC $ \(NumC n1) -> LamC $ \(NumC n2) -> BooC $ n1 <= n2)
    , ("=="       , LamC $ \(NumC n1) -> LamC $ \(NumC n2) -> BooC $ n1 == n2)
    , ("/="       , LamC $ \(NumC n1) -> LamC $ \(NumC n2) -> BooC $ n1 /= n2)
    , ("not"      , LamC $ \(BooC b) -> BooC $ not b)
    , ("or"       , LamC $ \b1 -> LamC $ \b2 -> orC b1 b2)
    , ("and"      , LamC $ \b1 -> LamC $ \b2 -> andC b1 b2)
    , ("chr"      , LamC $ \(NumC n) -> ChrC . chr $ round n)
    , ("ord"      , LamC $ \(ChrC c) -> NumC . fi $ ord c)
    , ("getArgs"  , getArgsC)
    , ("readFile" , LamC $ readFileC)
    , ("char?"    , LamC $ isChr)
    , ("nil?"     , LamC $ isNil)
    , ("par?"     , LamC $ isPar)
    , ("seq?"     , LamC $ isSeq)
    , ("car"      , LamC $ car)
    , ("cdr"      , LamC $ cdr)
    , ("par"      , LamC $ \x -> LamC $ \parxs -> parL x parxs)
    , ("seq"      , LamC $ \x -> LamC $ \seqxs -> seqL x seqxs)
    , ("time"     , LamC $ \e -> NumC $ getTime e)
    , ("pitch"    , LamC $ \e -> NumC $ getPitch e)
    , ("if"       , LamC $ \(BooC b) -> LamC $ \e1 -> LamC $ \e2 -> if b then e1 else e2)
    , ("true"     , BooC True)
    , ("false"    , BooC False) ]

andC :: ExprC -> ExprC -> ExprC
andC (BooC b1) b2c = BooC $ b1 && b2 where
    BooC b2 = b2c
andC _ _ = error $ "and : not boolean"

orC :: ExprC -> ExprC -> ExprC
orC (BooC b1) b2c = BooC $ b1 || b2 where
    BooC b2 = b2c
orC _ _ = error $ "or : not boolean"

parL :: ExprC -> ExprC -> ExprC
parL x parxs = ParC $ x:xs where
    xs = case parxs of
        ParC ys -> ys
        _ -> error "par: second argument is not par list"

seqL :: ExprC -> ExprC -> ExprC
seqL x seqxs = SeqC $ x:xs where
    xs = case seqxs of
        SeqC ys -> ys
        _ -> error "seq: second argument is not par list"

car :: ExprC -> ExprC
car (ParC (x:_)) = x
car (SeqC (x:_)) = x
car _ = error "car: List is empty"

cdr :: ExprC -> ExprC
cdr (ParC (_:xs)) = ParC xs
cdr (SeqC (_:xs)) = SeqC xs
cdr _ = error "cdr: List is empty"

getArgsC :: ExprC
getArgsC = SeqC . map (SeqC . map ChrC) . unsafePerformIO $ getArgs

readFileC :: ExprC -> ExprC
readFileC (SeqC cs) | all isC cs = SeqC . map ChrC . unsafePerformIO . readFile $ map getC cs where
    isC (ChrC _) = True
    isC _ = False
    getC (ChrC c) = c
    getC e = error $ "readFileC : getC : not a character : " ++ showExpr (variablePadding e)
readFileC e = error $ "readFileC : not a filename : " ++ showExpr (variablePadding e)

isChr :: ExprC -> ExprC
isChr (ChrC _) = BooC True
isChr _ = BooC False

isNil :: ExprC -> ExprC
isNil (ParC []) = BooC True
isNil (SeqC []) = BooC True
isNil _ = BooC False

isPar :: ExprC -> ExprC
isPar (ParC []) = BooC True
isPar _ = BooC False

isSeq :: ExprC -> ExprC
isSeq (SeqC []) = BooC True
isSeq _ = BooC False

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
getTime _ = error "argument to time is not valid"

getPitch :: ExprC -> Number
getPitch (NumC n) = n
getPitch (NoteC (NumC n) _) = n
getPitch _ = error "argument to pitch is not valid"

mapMelody :: (ExprC -> ExprC) -> ExprC -> ExprC
mapMelody f (SeqC es) = SeqC $ map (mapMelody f) es
mapMelody f (ParC es) = ParC $ map (mapMelody f) es
mapMelody f e = f e
