module Parser where

import Data.Char
import Data.List
import Data.Array

import Text.Parsec
import Text.Parsec.String

type Name = String
type Number = Double
type Defn = (Name, Expr)

data Expr = Var Name
          | Boo Bool
          | Num Number
          | Chr Char
          | Note Expr Expr
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
           | NoteB ExprB ExprB
           | AppB ExprB ExprB
           | SeqB [ExprB]
           | ParB [ExprB]
           | LamB ExprB
           | LetB [ExprB] ExprB

data ExprC = VarC Name
           | BooC Bool
           | NumC Number
           | ChrC Char
           | NoteC ExprC ExprC
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
    Note x1 x2 == Note y1 y2 = x1 == y1 && x2 == y2
    Seq xs == Seq ys = xs == ys
    Par xs == Par ys = xs == ys
    App _ _ == _ = error "First operand has not been fully evaluated when compared for equality"
    _ == App _ _ = error "Second operand has not been fully evaluated when compared for equality"
    Lam _ _ == _ = error "First operand is a function when compared for equality"
    _ == Lam _ _ = error "Second operand is a function when compared for equality"
    _ == _ = False

instance Show Expr where
    show = showExpr

showExpr :: Expr -> String
showExpr (Var x) = showAExpr (Var x)
showExpr (Boo b) = showAExpr (Boo b)
showExpr (Num n) | n >= 0 = show n
                 | otherwise = "- 0.0 " ++ show (-n)
showExpr (Chr c) = showAExpr (Chr c)
showExpr (Note e1 e2) = "(" ++ showExpr e1 ++ "," ++ showExpr e2 ++ ")"
showExpr (App e1 e2) = showFun e1 ++ " " ++ showArg e2 where
    showFun e = showExpr e
    showArg e = showAExpr e
showExpr (Seq es) = showAExpr (Seq es)
showExpr (Par es) = showAExpr (Par es)
showExpr (Lam x body) = "\\" ++ x ++ " -> " ++ showExpr body
showExpr (Let ds e) = "let " ++ intercalate " ; " (map showDefn ds) ++ " in " ++ showExpr e

showAExpr :: Expr -> String
showAExpr (Var x) = x
showAExpr (Boo True) = "true"
showAExpr (Boo False) = "false"
showAExpr (Num n) | n >= 0 = show n
                  | otherwise = "(- 0.0 " ++ show (-n) ++ ")"
showAExpr (Chr c) = show c
showAExpr (Seq cs) | all isC cs = show $ map getC cs where
    isC (Chr _) = True
    isC _ = False
    getC (Chr c) = c
    getC e = error $ "showAExpr : getC : not a character : " ++ showExpr e
showAExpr (Note e1 e2) = "(" ++ showExpr e1 ++ "," ++ showExpr e2 ++ ")"
showAExpr (Lam x body) = "(" ++ showExpr (Lam x body) ++ ")"
showAExpr (Seq es) = "[" ++ unwords (map showAExpr es) ++ "]"
showAExpr (Par es) = "{" ++ unwords (map showAExpr es) ++ "}"
showAExpr e = "(" ++ showExpr e ++ ")"

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
    Left err -> error (show err)
    Right prog -> prog

pProg :: Parser [Defn]
pProg = do
    prog <- pDefns
    eof
    return prog

pDefns :: Parser [Defn]
pDefns = (pSpaces >> pDefn >>= \d -> pSpaces >> return d) `sepBy1` char ';'

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
     <|> try pParentheseExpr
     <|> pNote

pChr :: Parser Expr
pChr = do
    _ <- char '\''
    c <- many $ noneOf "\'"
    _ <- char '\''
    return $ Chr . read $ "\'" ++ c ++ "\'"

pStr :: Parser Expr
pStr = do
    _ <- char '\"'
    str <- many $ noneOf "\""
    _ <- char '\"'
    return $ Seq . map Chr . read $ "\"" ++ str ++ "\""

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
    suffix <- (char '.' >> many1 digit) <|> return ""
    return $ Num . realToFrac . (read :: String -> Double) $ prefix ++ suffix

pOpExpr :: Parser Expr
pOpExpr = do
    op <- oneOf "^_#&"
    pSpaces
    e <- pAExpr
    return $ App (Var [op]) e

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
    e <- pExpr
    pSpaces
    _ <- char ')'
    return e

pNote :: Parser Expr
pNote = do
    _ <- char '('
    pSpaces
    e1 <- pExpr
    pSpaces
    _ <- char ','
    pSpaces
    e2 <- pExpr
    pSpaces
    _ <- char ')'
    return $ Note e1 e2

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
            c <- oneOf "+-*/%><" <|> letter
            if isLetter c
                then do
                    cs <- many (alphaNum <|> oneOf "_?")
                    return $ c:cs
                else do
                    cs <- string "=" <|> return ""
                    return $ c:cs

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
