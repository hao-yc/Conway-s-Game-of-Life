
{-# LANGUAGE OverloadedStrings #-}

module Develop.DatFw.Template.DocParsers
where
import Develop.DatFw.Template.Types
import Develop.DatFw.Template.ExprParsers

--import Data.Text (Text)
import qualified Data.Text as T
import Data.List as L
import Text.Parsec
import Text.Parsec.String

parseTemplate :: String -> Int -> Int -> String -> Either (SourcePos, [String]) [TplDoc]
parseTemplate sname sline scol input =
    case parse (templateP sname sline scol) "" input of
        Right docs -> Right docs
        Left perr ->
            let errls = L.drop 1 $ L.lines $ show perr
            in Left (errorPos perr, errls)

templateP :: String -> Int -> Int -> Parser [TplDoc]
templateP sname sline scol = do
    pos <- getPosition
    setPosition $ setSourceName (setSourceLine (setSourceColumn pos scol) sline) sname
    docsP <* eof

docsP :: Parser [TplDoc]
docsP = many docP

docP :: Parser TplDoc
docP =
    ifP <|> maybeP <|> forallP <|> withP <|> interpolP <|> rawP

ifP :: Parser TplDoc
ifP = do
    cond <- tagP "if" *> char '{' *> spaces *> exprP <* char '}'
    yes <- docsP
    no <- ifNextP
    pure (DocCond cond yes no)

ifNextP :: Parser [TplDoc]
ifNextP =
    do
        tagP "end"
        pure []
    <|> do
        tagP "else"
        docs <- docsP
        tagP "end"
        pure docs
    <|> do
        cond <- tagP "elseif" *> char '{' *> spaces *> exprP <* char '}'
        yes <- docsP
        no <- ifNextP
        pure [DocCond cond yes no]

maybeP :: Parser TplDoc
maybeP = do
    binding <- tagP "maybe" *> char '{' *> spaces *> generatorP <* char '}'
    yes <- docsP
    no <- maybeNextP
    pure $ DocMaybe binding yes no

maybeNextP :: Parser [TplDoc]
maybeNextP =
    do
        tagP "end"
        pure []
    <|> do
        tagP "nothing"
        no <- docsP
        tagP "end"
        pure no

forallP :: Parser TplDoc
forallP = do
    binding <- tagP "forall" *> char '{' *> spaces *> generatorP <* char '}'
    body <- docsP
    tagP "end"
    pure $ DocForall binding body

withP :: Parser TplDoc
withP = do
    binding <- tagP "with" *> char '{' *> spaces *> generatorP <* char '}'
    body <- docsP
    tagP "end"
    pure $ DocWith binding body

tagP :: String -> Parser ()
tagP i = try
    (do
        string ('$' : i)
        notFollowedBy alphaNum
      ) <?> ("\"$"++i++"\"")

generatorP :: Parser TplGenerator
generatorP = do
    bind <- patternP
    spaces
    string "<-"
    spaces
    value <- exprP
    pure (bind, value)

patternP :: Parser TplPattern
patternP = exprP

interpolP :: Parser TplDoc
interpolP = do
         (c, e) <- withBrackets
         case c of
                '#' -> pure $ DocHtmlE e
                '@' -> pure $ DocRouteE e
                _   -> pure $ DocEmbedE e
       <?> "interpolation"
  where
    withBrackets =
        (,) <$> try (satisfy interpolChar <* char '{') <*> (spaces *> exprP <* char '}')

rawP :: Parser TplDoc
rawP =
    DocRaw <$> ((:) <$> (satisfy interpolChar <|> satisfy notSpecialChar) <*> many (satisfy notSpecialChar))
    <?> "raw HTML"

interpolChar :: Char -> Bool
interpolChar c = c == '#' || c == '@' || c == '^'

notSpecialChar :: Char -> Bool
notSpecialChar c = c /= '$' && not (interpolChar c)

