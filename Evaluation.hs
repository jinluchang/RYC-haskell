module Evaluation where

import Data.List
import Data.Array
import Data.Char
import Control.DeepSeq

import System.Environment
import System.IO.Unsafe
import Debug.Trace

import Parser

fi :: (Integral a, Num b) => a -> b
fi x = fromIntegral x

interpret :: ([Name], EnvG) -> Expr -> ExprC
interpret (fns, envG) = eval envG [] . compile fns []

compile :: [Name] -> [Name] -> Expr -> ExprB
compile envG = go where
    go env expr = case expr of
        Boo b -> BooB b
        Num n -> NumB n
        Chr c -> ChrB c
        Note e1 e2 e3 -> NoteB (go env e1) (go env e2) (go env e3)
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

eval :: EnvG -> Env -> ExprB -> ExprC
eval envG = go where
    go env e = case e of
        VarB x -> VarC x
        BooB b -> BooC b
        NumB n -> NumC n
        ChrB c -> ChrC c
        SeqB es -> SeqC $ map (go env) es
        ParB es -> ParC $ map (go env) es
        NoteB e1 e2 e3 -> NoteC (go env e1) (go env e2) (go env e3)
        AppB e1 e2 -> apply (go env e1) (go env e2)
        LetB ds e' -> go env' e'
          where env' = map (go env') ds ++ env
        BoundB n -> env !! n
        BoundGB n -> envG ! n
        LamB body -> LamC $ \vx -> go (vx:env) body

apply :: ExprC -> ExprC -> ExprC
apply (LamC f) arg = f arg
apply fun arg = AppC fun arg

envGen :: [Defn] -> ([Name], EnvG)
envGen defns = nEnvG where
    nEnvG = nameEnvGCombine (definitionsGen defns nEnvG ++ primitivesGen nEnvG)

definitionsGen :: [Defn] -> ([Name], EnvG) -> [(Name, ExprC)]
definitionsGen defns (fns, envG) = zip dfns $ map (eval envG []) $ map (compile fns []) dfs  where
    (dfns, dfs) = unzip defns

nameEnvGCombine :: [(Name, ExprC)] -> ([Name], EnvG)
nameEnvGCombine env = (map fst env, listArray (0, length env - 1) $ map snd env)

variablePadding :: ExprC -> Expr
variablePadding = renameExpr . go names where
    go ns expr = case expr of
        VarC x -> Var x
        BooC b -> Boo b
        NumC n -> Num n
        ChrC c -> Chr c
        NoteC e1 e2 e3 -> Note (go ns e1) (go ns e2) (go ns e3)
        AppC e1 e2 -> App (go ns e1) (go ns e2)
        SeqC es -> Seq $ map (go ns) es
        ParC es -> Par $ map (go ns) es
        LamC f -> Lam (head ns) $ go (tail ns) $ f (VarC $ head ns)
    names = map ("$" ++) (map (\x->[x]) "abcdefghijklmnopqrstuvwxyz") ++
        map ("$var" ++) (map show ([0..] :: [Int]))

renameExpr :: Expr -> Expr
renameExpr = pad [] . compile [] [] where
    pad env eb = case eb of
        VarB y -> Var y
        BooB b -> Boo b
        NumB n -> Num n
        ChrB c -> Chr c
        SeqB es -> Seq $ map (pad env) es
        ParB es -> Par $ map (pad env) es
        NoteB e1 e2 e3 -> Note (pad env e1) (pad env e2) (pad env e3)
        AppB e1 e2 -> App (pad env e1) (pad env e2)
        LetB ds e' -> Let (zip ns $ map (pad env') ds) $ pad env' e'
          where
            ns = take (length ds) $ filter (`isNotFreeInAll` (e':ds)) $ filter (`notElem` env) names
            env' = ns ++ env
        BoundB n -> Var $ env !! n
        BoundGB _ -> error $ "global name in renaming, shouldn't happen"
        LamB body -> Lam n $ pad (n:env) body
          where n = head $ filter (`isNotFreeIn` body) $ filter (`notElem` env) names
    names = map (\x->[x]) "abcdefghijklmnopqrstuvwxyz" ++
        map ("var" ++) (map show ([0..] :: [Int]))

isNotFreeInAll :: Name -> [ExprB] -> Bool
x `isNotFreeInAll` exprBs = all (x `isNotFreeIn`) exprBs

isNotFreeIn :: Name -> ExprB -> Bool
x `isNotFreeIn` exprB = go exprB where
    go eb = case eb of
        VarB y -> x /= y
        BooB _ -> True
        NumB _ -> True
        ChrB _ -> True
        SeqB es -> all go es
        ParB es -> all go es
        NoteB e1 e2 e3 -> go e1 && go e2 && go e3
        AppB e1 e2 -> go e1 && go e2
        LetB ds e -> all go ds && go e
        BoundB _ -> True
        BoundGB _ -> True
        LamB body -> go body

-- ------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------

primitivesGen :: ([Name], EnvG) -> [(Name, ExprC)]
primitivesGen nameEnvG =
    [ ("@"          , LamC $ \f -> LamC $ \a -> apply f a)
    , ("^"          , LamC $ mapMelody $ raise 12)
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
    , ("number?"    , LamC $ BooC . isNum)
    , ("bool?"      , LamC $ BooC . isBoo)
    , ("char?"      , LamC $ BooC . isChr)
    , ("nil?"       , LamC $ BooC . isNil)
    , ("par?"       , LamC $ BooC . isPar)
    , ("seq?"       , LamC $ BooC . isSeq)
    , ("note?"      , LamC $ BooC . isNote)
    , ("function?"  , LamC $ BooC . isLam)
    , ("car"        , LamC $ car)
    , ("cdr"        , LamC $ cdr)
    , ("par"        , LamC $ \x -> LamC $ \parxs -> parL x parxs)
    , ("seq"        , LamC $ \x -> LamC $ \seqxs -> seqL x seqxs)
    , ("time"       , LamC $ \e -> NumC $ getTime e)
    , ("pitch"      , LamC $ \e -> NumC $ getPitch e)
    , ("velocity"   , LamC $ \e -> NumC $ getVelocity e)
    , ("if"         , LamC $ \b -> LamC $ \e1 -> LamC $ \e2 -> if getBooC b then e1 else e2)
    , ("true"       , BooC True)
    , ("false"      , BooC False)
    , ("show"       , LamC $ \e -> putStringC . showExprC $ e )
    , ("read"       , LamC $ \e -> interpret nameEnvG . readExpr $ getStringC e)
    , ("read-env"   , LamC $ \e -> putNEnvGC . envGen . readProg . getStringC $ e)
    , ("read-env-list"
                    , LamC $ \e -> putNEnvGC . envGen . concatMap (readProg . getStringC) $ getSeqC e)
    , ("read-env-gen"
                    , LamC $ \e -> LamC $ \envC ->
                        putNEnvC $ (definitionsGen . readProg . getStringC $ e) $ getNEnvGC envC)
    , ("read-with-env"
                    , LamC $ \envC ->
                        let env = getNEnvGC envC
                        in LamC $ \e -> interpret env . readExpr $ getStringC e)
    , ("global-env" , putNEnvGC nameEnvG)
    , ("empty-env"  , putNEnvGC emptyNameEnvG)
    , ("prim-env"   , putNEnvGC primitiveNameEnvG)
    , ("prim-env-gen"
                    , LamC $ \envC ->
                        putNEnvC $ primitivesGen $ getNEnvGC envC)
    , ("force"      , LamC $ \e -> deepseq e $ LamC id)
    , ("trace"      , LamC $ \e -> trace (map getChrC . getSeqC $ e) $ LamC id)
    , ("get-args"   , getArgsC)
    , ("read-file"  , LamC $ readFileC)
    , ("tempo"      , NumC 120) ]

primitiveNameEnvG :: ([Name], EnvG)
primitiveNameEnvG = nEnvG where
    nEnvG = nameEnvGCombine (primitivesGen nEnvG)

emptyNameEnvG :: ([Name], EnvG)
emptyNameEnvG = nameEnvGCombine []

getNEnvC :: ExprC -> [(Name, ExprC)]
getNEnvC = conv . transpose . map getParC . getSeqC where
    conv [fnsC, es] = zip (map getStringC fnsC) es
    conv [] = []
    conv _ = error $ "getNEnvC : not a environment"

getNEnvGC :: ExprC -> ([Name], EnvG)
getNEnvGC = nameEnvGCombine . getNEnvC

putNEnvC :: [(Name, ExprC)] -> ExprC
putNEnvC = SeqC . map (\(n,ec) -> ParC [putStringC n,ec])

putNEnvGC :: ([Name], EnvG) -> ExprC
putNEnvGC (fns, envG) = SeqC $ zipWith (\n e -> ParC [n,e]) fnsC $ elems envG where
    fnsC = map putStringC fns

putStringC :: String -> ExprC
putStringC = SeqC . map ChrC

getStringC :: ExprC -> String
getStringC = map getChrC . getSeqC

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
getArgsC = SeqC . map putStringC . unsafePerformIO $ getArgs

readFileC :: ExprC -> ExprC
readFileC (SeqC cs) | all isC cs = putStringC . unsafePerformIO . readFile $ map getChrC cs where
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

isNote :: ExprC -> Bool
isNote (NoteC _ _ _) = True
isNote _ = False

isLam :: ExprC -> Bool
isLam (LamC _) = True
isLam _ = False

isBoo :: ExprC -> Bool
isBoo (BooC _) = True
isBoo _ = False

isNum :: ExprC -> Bool
isNum (NumC _) = True
isNum _ = False

isChr :: ExprC -> Bool
isChr (ChrC _) = True
isChr _ = False

isNil :: ExprC -> Bool
isNil (ParC []) = True
isNil (SeqC []) = True
isNil _ = False

isPar :: ExprC -> Bool
isPar (ParC _) = True
isPar _ = False

isSeq :: ExprC -> Bool
isSeq (SeqC _) = True
isSeq _ = False

raise :: Int -> ExprC -> ExprC
raise k = mapMelody go where
    go (NoteC (NumC n) t v) = NoteC (NumC (loop k n)) t v
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

getPitch :: ExprC -> Number
getPitch (NumC n) = n
getPitch (NoteC (NumC n) _ _) = n
getPitch e = error $ "pitch : not a note : " ++ showExprC e

getTime :: ExprC -> Number
getTime (SeqC es) = sum $ map getTime es
getTime (ParC es) = maximum $ map getTime es
getTime (NumC _) = 1
getTime (NoteC _ (NumC t) _) = t
getTime e = error $ "time : not a melody : " ++ showExprC e

getVelocity :: ExprC -> Number
getVelocity (NumC _) = 127
getVelocity (NoteC _ _ (NumC v)) = v
getVelocity e = error $ "velocity : not a note : " ++ showExprC e

mapMelody :: (ExprC -> ExprC) -> ExprC -> ExprC
mapMelody f (SeqC es) = SeqC $ map (mapMelody f) es
mapMelody f (ParC es) = ParC $ map (mapMelody f) es
mapMelody f e = f e

showExprC :: ExprC -> String
showExprC = showExpr . variablePadding
