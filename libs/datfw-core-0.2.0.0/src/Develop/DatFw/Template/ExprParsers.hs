
{-# LANGUAGE OverloadedStrings #-}

module Develop.DatFw.Template.ExprParsers
where
import Develop.DatFw.Template.Types

import           Data.Char
import           Data.Functor.Identity (Identity)
import           Data.Maybe (listToMaybe)
import           Data.Text(Text)
import qualified Data.Text as T
import           Text.Parsec
import           Text.Parsec.String
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as P
import qualified Text.Read as R

exprP :: Parser TplExpr
exprP =
    (do
        pos <- getPosition
        e <- appExprPP
        opos <- getPosition
        ( do
            op <- operator
            let ope = if listToMaybe op == Just ':' then ExprCon opos op else ExprVar opos op
            ExprInfix pos e ope <$> exprP
          <|>
            pure e
          )
      ) <?> "expression"

appExprPP :: Parser TplExpr
appExprPP = do
    pos <- getPosition
    foldl1 (ExprApp pos) <$> many1 atomExprP

atomExprP :: Parser TplExpr
atomExprP = do
    pos <- getPosition
    do
        i <- identifier
        if maybe False isUpper $ listToMaybe i then pure $ ExprCon pos i else pure $ ExprVar pos i
      <|> ExprString pos <$> stringLiteral
      <|> (ExprNumber pos . either Left (Right . toRational)) <$> naturalOrFloat
      <|> ExprList pos <$> brackets (commaSep exprP)
      <|> do
        es <- parens $ commaSep exprP
        case es of
           [e] -> pure $ ExprParens pos e
           _ -> pure $ ExprTuple pos es


-- The lexer
lexer       = P.makeTokenParser tplExprDef

identifier     = P.identifier lexer
operator       = P.operator lexer
reserved       = P.reserved lexer
stringLiteral  = P.stringLiteral lexer
naturalOrFloat = P.naturalOrFloat lexer
parens         = P.parens lexer
brackets       = P.brackets lexer
commaSep       = P.commaSep lexer

tplExprDef :: P.GenLanguageDef String () Identity
tplExprDef = P.haskellDef
               { P.commentStart   = ""
               , P.commentEnd     = ""
               , P.commentLine    = ""
               }
old_tplExprDef    = P.LanguageDef
               { P.commentStart   = ""
               , P.commentEnd     = ""
               , P.commentLine    = ""
               , P.nestedComments = True
               , P.identStart     = letter <|> char '_'
               , P.identLetter    = alphaNum <|> oneOf "_'"
               , P.opStart        = P.opLetter tplExprDef
               , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , P.reservedOpNames= []
               , P.reservedNames  = []
               , P.caseSensitive  = True
               }

