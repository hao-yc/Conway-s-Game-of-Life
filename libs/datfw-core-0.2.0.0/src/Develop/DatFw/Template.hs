
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Develop.DatFw.Template
    ( htmlTempl, htmlTemplFile
    , widgetTempl, widgetTemplFile
    )
where
import Develop.DatFw.Template.DocParsers
import Develop.DatFw.Template.Conversions
import Develop.DatFw.Template.Types

import Develop.DatFw.Internal.Types
import Develop.DatFw.Widget
import Develop.DatFw.Handler

import System.IO as IO
import Data.Text as T
import Data.Text.IO as T

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Text.Blaze.Html (Html, toHtml)
import qualified Data.Map as M


htmlTempl :: QuasiQuoter
htmlTempl = do
    QuasiQuoter { quoteDec = const $ fail "Invalid quasi-quoter to generate declarations"
                , quoteType = const $ fail "Invalid quasi-quoter to generate types"
                , quotePat = const $ fail "Invalid quasi-quoter to generate patterns"
                , quoteExp = htmlTemplFromString }

htmlTemplFromString :: String -> Q Exp
htmlTemplFromString text = do
    Loc sname _ _ (sline, scol) _ <- location
    templFromString convertHtmlTempl sname sline scol text

htmlTemplFile :: FilePath -> Q Exp
htmlTemplFile fp = do
    addDependentFile fp
    s <- runIO (readUtf8File fp)
    templFromString convertHtmlTempl fp 1 1 $ T.unpack s

readUtf8File :: FilePath -> IO Text
readUtf8File fp = do
    IO.withFile fp IO.ReadMode $ \h -> do
        IO.hSetEncoding h utf8
#ifdef WINDOWS
        filter ('\r'/=)
#else
        id
#endif
            <$> T.hGetContents h


widgetTempl :: QuasiQuoter
widgetTempl = do
    QuasiQuoter { quoteDec = const $ fail "Invalid quasi-quoter to generate declarations"
                , quoteType = const $ fail "Invalid quasi-quoter to generate types"
                , quotePat = const $ fail "Invalid quasi-quoter to generate patterns"
                , quoteExp = widgetTemplFromString }

widgetTemplFromString :: String -> Q Exp
widgetTemplFromString text = do
    Loc sname _ _ (sline, scol) _ <- location
    templFromString convertWidgetTempl sname sline scol text

widgetTemplFile :: FilePath -> Q Exp
widgetTemplFile fp = do
    addDependentFile fp
    s <- runIO (readUtf8File fp)
    templFromString convertWidgetTempl fp 1 1 $ T.unpack s


convertHtmlTempl :: [TplDoc] -> Q Exp
convertHtmlTempl docs = do
    render <- newName "_render"
    let env = Env
            { envFromHtml = id
            , envFromRoute = \ r qs -> [| toHtml $ $(varE render) $r $qs |]
            , envEmbed = \ e -> [| $e $(varE render) |]
            , envMempty = [|memptyHtml|], envMappend = [|mappendHtml|]
            , envVars = M.empty
            }
    e <- tplDocsToExp env docs
    pure $ LamE [VarP render] e

memptyHtml :: Html
memptyHtml = mempty

mappendHtml :: Html -> Html -> Html
mappendHtml = mappend

convertWidgetTempl :: [TplDoc] -> Q Exp
convertWidgetTempl docs = do
    let env = Env
            { envFromHtml = \ e -> [| toWidget $e |]
            , envFromRoute = \ r qs -> [| getUrlRender >>= \ render -> toWidget $ toHtml $ render $r $qs |]
            , envEmbed = id
            , envMempty = [|memptyWidget|], envMappend = [|mappendWidget|]
            , envVars = M.empty
            }
    tplDocsToExp env docs

memptyWidget :: Widget site
memptyWidget = mempty

mappendWidget :: Widget site -> Widget site -> Widget site
mappendWidget = mappend


templFromString :: ([TplDoc] -> Q Exp) -> String -> Int -> Int -> String -> Q Exp
templFromString convert sname sline scol text =
    case parseTemplate sname sline scol text of
        Right docs -> convert docs
        Left (pos, errs) -> templateError pos errs

