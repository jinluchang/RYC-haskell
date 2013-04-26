module Parser where

import Data.List
import Data.Array
import Data.Ratio
import Control.DeepSeq

import Text.Parsec
import Text.Parsec.String

type Name = String
type Number = Rational
type Defn = (Name, Expr)

data Expr = Var Name
          | Boo Bool
          | Num Number
          | Chr Char
          | Note Expr Expr Expr
          | App Expr Expr
          | Seq [Expr]
          | Par [Expr]
          | Let [Defn] Expr
          | Lam Name Expr

data ExprB = VarB Name
           | BoundB Int
           | BoundGB Int
           | BooB Bool
           | NumB Number
           | ChrB Char
           | NoteB ExprB ExprB ExprB
           | AppB ExprB ExprB
           | SeqB [ExprB]
           | ParB [ExprB]
           | LamB ExprB
           | LetB [ExprB] ExprB

data ExprC = VarC Name
           | BooC Bool
           | NumC Number
           | ChrC Char
           | NoteC ExprC ExprC ExprC
           | AppC ExprC ExprC
           | SeqC [ExprC]
           | ParC [ExprC]
           | LamC (ExprC -> ExprC)

type Env = [ExprC]
type EnvG = Array Int ExprC

-- ------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------

instance Eq Expr where
    Var x == Var y = x == y
    Boo x == Boo y = x == y
    Num x == Num y = x == y
    Chr x == Chr y = x == y
    Note x1 x2 x3 == Note y1 y2 y3 = x1 == y1 && x2 == y2 && x3 == y3
    Seq xs == Seq ys = xs == ys
    Par xs == Par ys = xs == ys
    App _ _ == _ = error "First operand has not been fully evaluated when compared for equality"
    _ == App _ _ = error "Second operand has not been fully evaluated when compared for equality"
    Lam _ _ == _ = error "First operand is a function when compared for equality"
    _ == Lam _ _ = error "Second operand is a function when compared for equality"
    _ == _ = False

instance Eq ExprC where
    VarC x == VarC y = x == y
    BooC x == BooC y = x == y
    NumC x == NumC y = x == y
    ChrC x == ChrC y = x == y
    NoteC x1 x2 x3 == NoteC y1 y2 y3 = x1 == y1 && x2 == y2 && x3 == y3
    SeqC xs == SeqC ys = xs == ys
    ParC xs == ParC ys = xs == ys
    AppC _ _ == _ = error "First operand has not been fully evaluated when compared for equality"
    _ == AppC _ _ = error "Second operand has not been fully evaluated when compared for equality"
    LamC _ == _ = error "First operand is a function when compared for equality"
    _ == LamC  _ = error "Second operand is a function when compared for equality"
    _ == _ = False

instance NFData ExprC where
    rnf (VarC x) = rnf x
    rnf (BooC x) = rnf x
    rnf (NumC x) = rnf x
    rnf (ChrC x) = rnf x
    rnf (NoteC x y z) = rnf (x,y,z)
    rnf (SeqC xs) = rnf xs
    rnf (ParC xs) = rnf xs
    rnf (AppC x y) = rnf (x,y)
    rnf (LamC _) = ()

instance Show Expr where
    show = showExpr

showExpr :: Expr -> String
showExpr (Var x) = showAExpr $ Var x
showExpr (Boo b) = showAExpr $ Boo b
showExpr (Num n) | n < 0 = "- 0 " ++ showAExpr (Num (-n))
                 | 1000 `mod` denominator n /= 0 = "/ " ++ show (numerator n) ++ " " ++ show (denominator n)
                 | otherwise = showAExpr (Num n)
showExpr (Chr c) = showAExpr (Chr c)
showExpr (Note e1 e2 e3) = showAExpr $ Note e1 e2 e3
showExpr (App e1 e2) = showFun e1 ++ " " ++ showArg e2 where
    showFun e = showExpr e
    showArg e = showAExpr e
showExpr (Seq es) = showAExpr $ Seq es
showExpr (Par es) = showAExpr $ Par es
showExpr (Lam x body) = showLamExpr [x] body
showExpr (Let ds e) = "let " ++ intercalate " ; " (map showDefn ds) ++ " in " ++ showExpr e

showAExpr :: Expr -> String
showAExpr (Var x) = x
showAExpr (Boo True) = "true"
showAExpr (Boo False) = "false"
showAExpr (Num n) | n < 0 = "(" ++ showExpr (Num n) ++ ")"
                  | denominator n == 1 = show $ numerator n
                  | 1000 `mod` denominator n == 0 = show $ (realToFrac :: Number -> Double) n
                  | otherwise = "(" ++ showExpr (Num n) ++ ")"
showAExpr (Chr c) = show c
showAExpr (Seq []) = "[]"
showAExpr (Seq cs) | all isC cs = show $ map getC cs where
    isC (Chr _) = True
    isC _ = False
    getC (Chr c) = c
    getC e = error $ "showAExpr : getC : not a character : " ++ showExpr e
showAExpr (Note e1 e2 (Num 127)) = "(" ++ showExpr e1 ++ "," ++ showExpr e2 ++ ")"
showAExpr (Note e1 e2 e3) = "(" ++ showExpr e1 ++ "," ++ showExpr e2 ++ "," ++ showExpr e3 ++ ")"
showAExpr (Seq es) = "[" ++ unwords (map showAExpr es) ++ "]"
showAExpr (Par es) = "{" ++ unwords (map showAExpr es) ++ "}"
showAExpr e = "(" ++ showExpr e ++ ")"

showLamExpr :: [Name] -> Expr -> String
showLamExpr xs (Lam x body) = showLamExpr (x:xs) body
showLamExpr xs body = "\\" ++ unwords (reverse xs) ++ " -> " ++ showExpr body

showDefn :: Defn -> String
showDefn (fun, defn) = go [] defn where
    go xs (Lam x body) = go (x:xs) body
    go [] body = fun ++ " = " ++ showExpr body
    go xs body = fun ++ " " ++ unwords (reverse xs) ++ " = " ++ showExpr body

showDefns :: [Defn] -> String
showDefns = (++"\n") . intercalate " ;\n" . map showDefn

-- ------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------

readProg :: String -> [Defn]
readProg str = case parse pProg (take 10 str) str of
    Left err -> error $ show err
    Right prog -> prog

readExpr :: String -> Expr
readExpr str = case parse pExpr (take 10 str) str of
    Left err -> error $ show err
    Right expr -> expr

pProg :: Parser [Defn]
pProg = do
    prog <- pDefns
    eof
    return prog

pDefns :: Parser [Defn]
pDefns = do
    pSpaces
    ds <- padSpaces pDefn `sepEndBy1` padSpaces (char ';')
    return ds

pDefn :: Parser Defn
pDefn = do
    fun <- pName
    pSpaces
    vars <- many (pName >>= \var -> pSpaces >> return var)
    _ <- char '='
    pSpaces
    e <- pExpr
    return $ (fun, genFun vars e)
  where
    genFun [] = id
    genFun (x:xs) = Lam x . genFun xs

pExpr :: Parser Expr
pExpr = (pAExpr >>= (\e -> pSpaces >> return e)) `chainl1` return App

pAExpr :: Parser Expr
pAExpr = pLet
     <|> pLam
     <|> pVar
     <|> pNum
     <|> pChr
     <|> pStr
     <|> pOpExpr
     <|> pSeqList
     <|> pParList
     <|> pParentheseExpr

pChr :: Parser Expr
pChr = do
    _ <- char '\''
    str <- pCharSpecial '\''
    return $ Chr . (\[c] -> c) $ str

pStr :: Parser Expr
pStr = do
    _ <- char '\"'
    str <- pCharSpecial '\"'
    return $ Seq . map Chr $ str

pCharSpecial :: Char -> Parser String
pCharSpecial t = go where
    go = do
        c <- anyChar
        case c of
            '\\' -> do
                c' <- anyChar
                cs <- go
                return $ read ("\'\\" ++ [c'] ++ "\'") : cs
            _ | c == t -> return ""
              | otherwise -> do
                cs <- go
                return $ c : cs

pLet :: Parser Expr
pLet = do
    _ <- try $ string "let"
    defns <- pDefns
    _ <- string "in"
    pSpaces
    expr <- pExpr
    return $ Let defns expr

pLam :: Parser Expr
pLam = do
    _ <- char '\\'
    vars <- many1 (try $ pSpaces >> pName)
    pSpaces
    _ <- string "->"
    pSpaces
    body <- pExpr
    return $ go vars body
  where
    go [] = id
    go (x:xs) = Lam x . go xs

pVar :: Parser Expr
pVar = do
    x <- pName
    return $ Var x

pNum :: Parser Expr
pNum = do
    prefix <- many1 digit
    suffix <- (char '.' >> many1 digit >>= \cs -> return $ '.':cs) <|> return ""
    return $ Num . (\x -> approxRational x (x*1.0E-10)) . (read :: String -> Double) $ prefix ++ suffix

pOpExpr :: Parser Expr
pOpExpr = do
    op <- oneOf "^_#&@"
    if op /= '@'
        then do
            pSpaces
            e <- pAExpr
            return $ App (Var [op]) e
        else do
            f <- pName
            pSpaces
            e <- pAExpr
            return $ App (Var f) e

pSeqList :: Parser Expr
pSeqList = do
    _ <- char '['
    pSpaces
    es <- many (pAExpr >>= \e ->
        pSpaces >>
        ((char '|' >> pSpaces) <|> return ()) >>
        return e)
    _ <- char ']'
    return $ Seq es

pParList :: Parser Expr
pParList = do
    _ <- char '{'
    pSpaces
    es <- many (pAExpr >>= \e ->
        pSpaces >>
        ((char '|' >> pSpaces) <|> return ()) >>
        return e)
    _ <- char '}'
    return $ Par es

pParentheseExpr :: Parser Expr
pParentheseExpr = do
    _ <- char '('
    pSpaces
    es <- padSpaces pExpr `sepBy1` padSpaces (char ',')
    _ <- char ')'
    case es of
        [e] -> return e
        [e1,e2] -> return $ Note e1 e2 (Num 127)
        [e1,e2,e3] -> return $ Note e1 e2 e3
        _ -> fail "invalid parenthese expression"

pName :: Parser Name
pName = do
    bad <- lookAhead (try (string "->" <|> string "let" <|> string "in") <|> return "")
    if bad /= ""
        then fail "Keywords"
        else return ()
    op <- lookAhead (try (string "==" <|> string "||" <|> string "&&") <|> return "")
    if op /= ""
        then string op
        else do
            c <- oneOf "+-*/<>" <|> letter
            cs <- many (alphaNum <|> oneOf "=_?\'+-*/<>")
            return $ c:cs

padSpaces :: Parser a -> Parser a
padSpaces p = do
    x <- p
    pSpaces
    return x

pSpaces :: Parser ()
pSpaces = do
    spaces
    pComment <|> return ()

pComment :: Parser ()
pComment = do
    _ <- try $ string "/*"
    pCommentRest
  where
    pCommentRest = do
        c <- anyChar
        if c /= '*'
            then pCommentRest
            else do
                c' <- anyChar
                if c' /= '/'
                    then pCommentRest
                    else pSpaces
