
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Develop.DatFw.Template.Conversions
where
import Develop.DatFw.Template.Types

import           Data.Text(Text)
import qualified Data.Text as T
import           Text.Read
import           Text.Parsec
import           Text.Parsec.Text
import           Text.Blaze.Html
import qualified Data.Map as M

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

{--
        urender :: (url -> [(String,String)] -> String)
        urender -> id(HTML) <> id(Text(urender ROUTE [])) <> EMBED urender

        getUrlRender >>= \ urender' -> toWidget HTML <> toWidget (Text (urender' ROUTE [])) <> EMBED
        (wd) -> {
                urender = getUrlRender(wd.hdata);
                toWidgetHtml HTML wd *> toWidgetHtml (Text (urender ROUTE [])) wd *> EMBED.run wd
                return null;
        }
--}
data Env = Env
        { envFromHtml :: Q Exp -> Q Exp         -- HTML -> MONOID
        , envFromRoute :: Q Exp -> Q Exp -> Q Exp -- Route a -> [(Text,Text)] -> MONOID
        , envEmbed :: Q Exp -> Q Exp            -- WIDGET -> MONOID
        , envMempty :: Q Exp                    -- MONOID dictionary
        , envMappend :: Q Exp
        , envVars :: M.Map String Name
        }

---------------------------------------------------------------------------------
-- Convert to HTML monoid

tplDocsToExp :: Env -> [TplDoc] -> Q Exp
tplDocsToExp env [] =
    envMempty env
tplDocsToExp env [d] =
    tplDocToExp env d
tplDocsToExp env (d:ds) =
    [| $(envMappend env) $(tplDocToExp env d) $(tplDocsToExp env ds) |]

-- Convert to HTML build code
tplDocToExp :: Env -> TplDoc -> Q Exp
tplDocToExp env (DocForall (b, e) ds) =
    tplExprToPat env b $ \ env2 pat ->
        [| foldMap (\ $pat -> $(tplDocsToExp env2 ds)) $(tplExprToExp env e) |]

tplDocToExp env (DocCond e yes no) =
    [| if $(tplExprToExp env e) then $(tplDocsToExp env yes) else $(tplDocsToExp env no) |]

tplDocToExp env (DocMaybe (b, e) yes no) =
    tplExprToPat env b $ \ env2 pat ->
        [| maybe $(tplDocsToExp env no) (\ $pat -> $(tplDocsToExp env2 yes)) $(tplExprToExp env e) |]

tplDocToExp env (DocWith (b, e) ds) =
    tplExprToPat env b $ \ env2 pat ->
        [| let { $pat = $(tplExprToExp env e) } in $(tplDocsToExp env2 ds) |]

tplDocToExp env (DocRaw s) =
    envFromHtml env [| preEscapedText (T.pack s) |]

tplDocToExp env (DocHtmlE e) =
    envFromHtml env [| toHtml $(tplExprToExp env e) |]

tplDocToExp env (DocRouteE e) =
    envFromRoute env (tplExprToExp env e) [| [] |]

tplDocToExp env (DocEmbedE e) =
    envEmbed env (tplExprToExp env e)


---------------------------------------------------------------------------------
-- Lift template patterns

tplExprToPat :: Env -> TplExpr -> (Env -> Q Pat -> Q a) -> Q a
tplExprToPat env expr inside = do
    tplExprToPat' expr $ \ bindings qpat ->
        inside env{ envVars =  foldr (uncurry M.insert) (envVars env) bindings } qpat

tplExprToPat' :: TplExpr -> ([(String, Name)] -> Q Pat -> Q a) -> Q (a)
tplExprToPat' (ExprVar _ s) inside = do
    nm <- newName s
    inside [(s, nm)] $ varP nm

tplExprToPat' (ExprCon _ s) inside = do
    nm <- newName s
    inside [(s, nm)] $ conP nm []

tplExprToPat' (ExprString _ s) inside =
    inside [] $ litP $ stringL s

tplExprToPat' (ExprNumber _ (Left i)) inside =
    inside [] $ litP $ integerL i

tplExprToPat' (ExprNumber _ (Right r)) inside =
    inside [] $ litP $ rationalL r

tplExprToPat' e@(ExprApp _ _ _) inside = do
    go [] e []
    where
        go nms (ExprApp _ e1 e2) pats =
            tplExprToPat' e2 $ \ nms2 pat2 -> go (nms2 ++ nms) e1 (pat2 : pats)
        go nms (ExprCon pos s) pats = do
            n <- lookupConstr pos s
            inside nms $ conP n pats
        go nms e pats =
            templateError (getExprPos e) ["Expecting a constructor name or a pattern application"]

tplExprToPat' (ExprInfix _ e1 op e2) inside =
    tplExprToPat' e1 $ \ nms1 pat1 ->
        tplExprToPat' e2 $ \ nms2 pat2 ->
            case op of
                ExprCon opos s -> do
                    n <- lookupConstr opos s
                    inside (nms1 ++ nms2) $ uInfixP pat1 n pat2
                _ ->
                    templateError (getExprPos op) ["Expecting a constructor name"]

tplExprToPat' (ExprTuple _ es) inside = do
    go [] [] es
    where
        go nms rpats (e1:es2) =
            tplExprToPat' e1 $ \ nms1 pat -> go (nms1 ++ nms) (pat : rpats) es2
        go nms rpats [] =
            inside nms $ tupP $ reverse rpats

tplExprToPat' (ExprList _ es) inside = do
    go [] [] es
    where
        go nms rpats (e1:es2) =
            tplExprToPat' e1 $ \ nms1 pat -> go (nms1 ++ nms) (pat : rpats) es2
        go nms rpats [] =
            inside nms $ listP $ reverse rpats

tplExprToPat' (ExprParens _ e) inside =
    tplExprToPat' e $ \ nms pat -> inside nms $ parensP pat

---------------------------------------------------------------------------------
-- Lift template expressions

tplExprToExp :: Env -> TplExpr -> Q Exp
tplExprToExp env (ExprVar pos s) =
    case M.lookup s (envVars env) of
        Just n -> varE n
        Nothing -> do
            mbn <- lookupValueName s
            case mbn of
                Nothing -> templateError pos ["No variable '" ++ s ++ "' in scope at the site of the splice"]
                Just n -> varE n

tplExprToExp env (ExprCon pos s) = do
    n <- lookupConstr pos s
    conE n

tplExprToExp env (ExprString pos s) =
    litE $ stringL s

tplExprToExp env (ExprNumber pos (Left i)) =
    litE $ integerL i

tplExprToExp env (ExprNumber pos (Right r)) =
    litE $ rationalL r

tplExprToExp env (ExprApp pos e1 e2) =
    appE (tplExprToExp env e1) (tplExprToExp env e2)

tplExprToExp env (ExprInfix pos e1 op e2) =
    uInfixE (tplExprToExp env e1) (tplExprToExp env op) (tplExprToExp env e2)

tplExprToExp env (ExprList pos es) =
    listE $ map (tplExprToExp env) es

tplExprToExp env (ExprTuple pos es) =
    tupE $ map (tplExprToExp env) es

tplExprToExp env (ExprParens pos e) =
    parensE $ tplExprToExp env e


---------------------------------------------------------------------------------
-- Utilities

lookupConstr :: SourcePos -> String -> Q Name
lookupConstr pos ":" =
    pure $ mkName ":"
lookupConstr pos s = do
    mbn <- lookupValueName s
    case mbn of
        Nothing -> templateError pos ["No constructor '" ++ s ++ "' in scope at the site of the splice"]
        Just n -> pure n

templateError :: SourcePos -> [String] -> Q a
templateError pos ss =
    fail $ "Template error in " ++ show pos ++ ":"
         ++ mconcat (("\n        " ++) <$> ss)

