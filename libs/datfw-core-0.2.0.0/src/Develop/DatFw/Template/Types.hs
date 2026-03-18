
{-# LANGUAGE OverloadedStrings #-}

module Develop.DatFw.Template.Types
where
import Text.Parsec.Pos


data TplDoc = DocWith TplGenerator [TplDoc]
            | DocForall TplGenerator [TplDoc]
            | DocCond TplExpr [TplDoc] [TplDoc]
            | DocMaybe TplGenerator [TplDoc] [TplDoc]
            | DocRaw String
            | DocHtmlE TplExpr
            | DocRouteE TplExpr
            | DocEmbedE TplExpr

type TplGenerator = (TplPattern, TplExpr)

type TplPattern = TplExpr

data TplExpr = ExprVar SourcePos String
             | ExprCon SourcePos String
             | ExprString SourcePos String
             | ExprNumber SourcePos (Either Integer Rational)
             | ExprApp SourcePos TplExpr TplExpr
             | ExprInfix SourcePos TplExpr TplExpr TplExpr
             | ExprTuple SourcePos [TplExpr]
             | ExprList SourcePos [TplExpr]
             | ExprParens SourcePos TplExpr
        deriving (Eq, Show)

getExprPos :: TplExpr -> SourcePos
getExprPos (ExprVar pos _) = pos
getExprPos (ExprCon pos _) = pos
getExprPos (ExprString pos _) = pos
getExprPos (ExprNumber pos _) = pos
getExprPos (ExprApp pos _ _) = pos
getExprPos (ExprInfix pos _ _ _) = pos
getExprPos (ExprTuple pos _) = pos
getExprPos (ExprList pos _) = pos
getExprPos (ExprParens pos _) = pos

