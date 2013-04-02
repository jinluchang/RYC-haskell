module Parser where

import Data.Char

import Text.Parsec
import Text.Parsec.String

type Name = String
type Number = Double
type Defn = (Name, Func)

type Env = [(Name, (Int, [Expr] -> Expr))]

data Func = Func [Name] Expr

data Expr = Var Name
          | Boo Bool
          | Num Number
          | Note Expr Expr
          | App Expr Expr
          | Seq [Expr]
          | Par [Expr]
          | Fun Int ([Expr] -> Expr)

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
    Fun _ _ == _ = error "First operand is a function when compared for equality"
    _ == Fun _ _ = error "Second operand is a function when compared for equality"
    _ == _ = False

instance Show Expr where
    show = showExpr

showExpr :: Expr -> String
showExpr (Var x) = x
showExpr (Boo b) = show b
showExpr (Num n) | n >= 0 = show n
                 | otherwise = "- 0.0 " ++ show (-n)
showExpr (Note e1 e2) = "(" ++ showExpr e1 ++ "," ++ showExpr e2 ++ ")"
showExpr (App e1 e2) = showFun e1 ++ " " ++ showArg e2 where
    showFun e = showExpr e
    showArg e = showAExpr e
showExpr (Seq es) = "[" ++ unwords (map showAExpr es) ++ "]"
showExpr (Par es) = "{" ++ unwords (map showAExpr es) ++ "}"
showExpr (Fun _ _) = "FUNCTION"

showAExpr :: Expr -> String
showAExpr (Var x) = x
showAExpr (Num n) | n >= 0 = show n
                  | otherwise = "(- 0.0 " ++ show (-n) ++ ")"
showAExpr (Boo b) = show b
showAExpr (Note e1 e2) = "(" ++ showExpr e1 ++ "," ++ showExpr e2 ++ ")"
showAExpr (Fun _ _) = "FUNCTION"
showAExpr (Seq es) = "[" ++ unwords (map showAExpr es) ++ "]"
showAExpr (Par es) = "{" ++ unwords (map showAExpr es) ++ "}"
showAExpr e = "(" ++ showExpr e ++ ")"

showDefns :: [Defn] -> String
showDefns = unlines . map showDefn

showDefn :: Defn -> String
showDefn (fun, Func [] body) = fun ++ " = " ++ showExpr body
showDefn (fun, Func vars body) = fun ++ " " ++ unwords vars ++ " = " ++ showExpr body

-- ------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------

readProg :: String -> [Defn]
readProg str = case parse pProg (take 10 str) str of
    Left err -> error (show err)
    Right prog -> prog

pProg :: Parser [Defn]
pProg = do
    prog <- (pSpaces >> pDefn >>= \d -> pSpaces >> return d) `sepBy1` char ';'
    eof
    return prog

pDefn :: Parser Defn
pDefn = do
    fun <- pName
    pSpaces
    vars <- many (pName >>= \var -> pSpaces >> return var)
    _ <- char '='
    pSpaces
    e <- pExpr
    return $ (fun, Func vars e)

pExpr :: Parser Expr
pExpr = (pAExpr >>= (\e -> pSpaces >> return e)) `chainl1` return App

pAExpr :: Parser Expr
pAExpr = pVar
     <|> pNum
     <|> pOpExpr
     <|> pSeqList
     <|> pParList
     <|> try pParentheseExpr
     <|> pNote

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
    op <- lookAhead (try (string "==") <|> return "")
    if op == "=="
        then string "=="
        else do
            c <- oneOf "+-*/%><" <|> letter
            if isLetter c
                then do
                    cs <- many (alphaNum <|> oneOf "?")
                    return $ c:cs
                else do
                    cs <- string "=" <|> return ""
                    return $ c:cs

pSpaces :: Parser ()
pSpaces = do
    spaces
    pComment

pComment :: Parser ()
pComment = do
    start <- try (string "/*") <|> return ""
    if null start
        then return ()
        else pCommentRest
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
