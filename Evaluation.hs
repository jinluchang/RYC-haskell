module Evaluation where

import Parser

compile :: Env -> Func -> (Int, [Expr] -> Expr)
compile env (Func vs body) = (length vs, \as -> eval env (zip vs as) body)

envGen :: [Defn] -> Env
envGen defns = env where
    (fns, fs) = unzip defns
    env = zip fns (map (compile env) fs) ++ primitives

eval :: Env -> [(Name, Expr)] -> Expr -> Expr
eval env bs expr = go expr where
    apply (Fun n f) a = case n of
        0 -> error "function do not accept any argument"
        1 -> go $ f [a]
        _ -> Fun (n-1) $ \as -> f (a:as)
    apply f a = error $ "cannot apply " ++ showExpr f ++ " to " ++ showExpr a
    go (App e1 e2) = apply (go e1) (go e2)
    go (Var x) = case lookup x bs of
        Just e -> go e
        Nothing -> case lookup x env of
            Nothing -> error $ "Unbounded variable: " ++ x
            Just (n, f) -> go $ Fun n f
    go (Note e1 e2) = Note (go e1) (go e2)
    go (Seq es) = Seq $ map go es
    go (Par es) = Par $ map go es
    go (Fun 0 f) = go $ f []
    go x = x

-- ------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------

primitives :: Env
primitives =
    [ ("^"      , (1, liftSingleFunc $ mapMelody $ raise 12))
    , ("_"      , (1, liftSingleFunc $ mapMelody $ raise (-12)))
    , ("#"      , (1, liftSingleFunc $ mapMelody $ raise 1))
    , ("&"      , (1, liftSingleFunc $ mapMelody $ raise (-1)))
    , ("/"      , (2, \[Num n1, Num n2] -> Num $ n1 / n2))
    , ("*"      , (2, \[Num n1, Num n2] -> Num $ n1 * n2))
    , ("+"      , (2, \[Num n1, Num n2] -> Num $ n1 + n2))
    , ("-"      , (2, \[Num n1, Num n2] -> Num $ n1 - n2))
    , (">"      , (2, \[Num n1, Num n2] -> Boo $ n1 > n2))
    , ("<"      , (2, \[Num n1, Num n2] -> Boo $ n1 < n2))
    , (">="     , (2, \[Num n1, Num n2] -> Boo $ n1 >= n2))
    , ("<="     , (2, \[Num n1, Num n2] -> Boo $ n1 <= n2))
    , ("=="     , (2, \[e1,e2] -> Boo $ e1 == e2))
    , ("/="     , (2, \[e1,e2] -> Boo $ e1 /= e2))
    , ("not"    , (1, \[Boo b] -> Boo $ not b))
    , ("or"     , (2, \[Boo b1, Boo b2] -> Boo $ b1 || b2))
    , ("and"    , (2, \[Boo b1, Boo b2] -> Boo $ b1 && b2))
    , ("nil?"   , (1, liftSingleFunc isNil))
    , ("par?"   , (1, liftSingleFunc isPar))
    , ("seq?"   , (1, liftSingleFunc isSeq))
    , ("car"    , (1, liftSingleFunc car))
    , ("cdr"    , (1, liftSingleFunc cdr))
    , ("par"    , (2, parL))
    , ("seq"    , (2, seqL))
    , ("time"   , (1, \[e] -> Num $ getTime e))
    , ("pitch"  , (1, \[e] -> Num $ getPitch e))
    , ("if"     , (3, \[Boo b, e1, e2] -> if b then e1 else e2)) ]

liftSingleFunc :: (Expr -> Expr) -> [Expr] -> Expr
liftSingleFunc f [e] = f e
liftSingleFunc _ _ = error "number of argument is not correct in liftSingleFunc"

parL :: [Expr] -> Expr
parL [x, parxs] = Par (x:xs) where
    xs = case parxs of
        Par ys -> ys
        _ -> error "par: second argument is not par list"
parL _ = error "par: number of argument is not correct"

seqL :: [Expr] -> Expr
seqL [x, seqxs] = Seq (x:xs) where
    xs = case seqxs of
        Seq ys -> ys
        _ -> error "seq: second argument is not par list"
seqL _ = error "seq: number of argument is not correct"

car :: Expr -> Expr
car (Par (x:_)) = x
car (Seq (x:_)) = x
car _ = error "car: List is empty"

cdr :: Expr -> Expr
cdr (Par (_:xs)) = Par xs
cdr (Seq (_:xs)) = Seq xs
cdr _ = error "cdr: List is empty"

isNil :: Expr -> Expr
isNil (Par []) = Boo True
isNil (Seq []) = Boo True
isNil _ = Boo False

isPar :: Expr -> Expr
isPar (Par []) = Boo True
isPar _ = Boo False

isSeq :: Expr -> Expr
isSeq (Seq []) = Boo True
isSeq _ = Boo False

raise :: Int -> Expr -> Expr
raise k = mapMelody go where
    go (Note (Num n) t) = Note (Num (loop k n)) t
    go (Num n) = Num (loop k n)
    go _ = error "change pitch is not allowed for thish argument"
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

getTime :: Expr -> Number
getTime (Seq es) = sum $ map getTime es
getTime (Par es) = maximum $ map getTime es
getTime (Num _) = 1
getTime (Note _ (Num t)) = t
getTime _ = error "argument to time is not valid"

getPitch :: Expr -> Number
getPitch (Num n) = n
getPitch (Note (Num n) _) = n
getPitch _ = error "argument to pitch is not valid"

mapMelody :: (Expr -> Expr) -> Expr -> Expr
mapMelody f (Seq es) = Seq $ map (mapMelody f) es
mapMelody f (Par es) = Par $ map (mapMelody f) es
mapMelody f e = f e
