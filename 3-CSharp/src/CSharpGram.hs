module CSharpGram where

import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex
import Prelude hiding ((<$), (<*), (*>), sequence, Left, Right)

data Class = Class String [Member]
           deriving Show

data Member = MemberD Decl
            | MemberM Type String [Decl] Stat
            deriving Show

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatReturn Expr
          | StatBlock  [Stat]
          deriving Show

{- 
    NOTE: Unlike regular C#, this won't do typechecking yet
    NOTE: the precedence for boolean operations (from high to low) is: &, ^, |, &&, ||.
    Here && and || are lazy variants of & and |. In our implementation we will use the precedence: ^, &&, ||.  
-}
data Expr = ExprConst  Int
          | ExprVar    String
          | ExprOper   String Expr Expr
          deriving Show

data Decl = Decl Type String
          deriving Show

data Type = TypeVoid
          | TypePrim  String
          | TypeObj   String
          deriving (Eq,Show)


pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> TypeVoid <$ symbol KeyVoid
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =  StatExpr <$> pExpr <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pExpr <*> pStat <*> optionalElse
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr <*> pStat
     <|> StatReturn <$ symbol KeyReturn <*> pExpr               <*  sSemi
     <|> (\a b c d -> StatBlock [a, StatWhile b (StatBlock [d,c])]) <$ symbol KeyFor <* symbol POpen 
       <*> exprDecls <* sSemi <*> pExpr <* sSemi <*> exprDecls <* symbol PClose <*> pStat
     <|> pBlock
     where optionalElse = option (symbol KeyElse *> pStat) (StatBlock [])
exprDecls :: Parser Token Stat
exprDecls = StatBlock <$> option (listOf (StatExpr <$> pExpr <|> StatDecl <$> pDecl) sComma) []

--Parses something which can be treated as a single variable for the pExpr function
pExprSimple :: Parser Token Expr
pExprSimple =  ExprConst <$> (sConstInt <|> sConstChar <|> sConstBool)
           <|> ExprVar   <$> sLowerId
           <|> parenthesised pExpr

data Direction = Left | Right

priority :: [(String,Direction)]
priority = [("*",Left),("/",Left),("%",Left),
           ("+",Left),("-",Left),("<",Left),
           (">",Left),("<=",Left),(">=",Left),
           ("==",Left),("!=",Left),("^",Left),
           ("&&",Left),("||",Left),("=",Right)]

pExpr :: Parser Token Expr
pExpr = foldl f pExprSimple priority
  where f parser (newOperator,Left) = chainl parser (ExprOper <$> sOperator newOperator)
        f parser (newOperator,Right) = chainr parser (ExprOper <$> sOperator newOperator)

testExpr :: [Token]
testExpr = fst $ head $ parse lexicalScanner "2+3*4"

pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = pDecl <* sSemi

pType :: Parser Token Type
pType =  TypePrim <$> sStdType
     <|> TypeObj  <$> sUpperId


-- The `Token` equivalents to some basic parser combinators
parenthesised, bracketed, braced :: Parser Token b -> Parser Token b
parenthesised p = pack (symbol POpen) p (symbol PClose) --(p)
bracketed     p = pack (symbol SOpen) p (symbol SClose) --[p]
braced        p = pack (symbol COpen) p (symbol CClose) --{p}