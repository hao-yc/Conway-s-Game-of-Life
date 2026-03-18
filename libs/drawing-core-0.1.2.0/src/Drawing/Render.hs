
{-# LANGUAGE OverloadedStrings #-}

module Drawing.Render
    ( unitSize
    , SvgTree(..), SvgAttr, renderSvgTree
    , renderSvgText, renderSvgText'
    )
where
import           Drawing.Types

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B


-- 'viewXMax' and 'viewYMax' are in pixels
renderSvgText :: Int -> Int -> Drawing -> (Double, TL.Text)
renderSvgText viewXMax viewYMax draw =
    B.toLazyText <$> renderSvgText' viewXMax viewYMax draw

renderSvgText' :: Int -> Int -> Drawing -> (Double, B.Builder)
renderSvgText' viewXMax viewYMax draw =
    ( unitSize
    , toSvgBuilder (renderSvgTree viewXMax viewYMax draw)
    )

toSvgBuilder :: SvgTree -> B.Builder
toSvgBuilder (SvgTree name attrs children) =
    case children of
        [SvgText txt] ->
            "<" <> B.fromText name <> mconcat (addAttr <$> attrs) <> ">"
                <> escapeHtml txt <>
            "</" <> B.fromText name <> ">\n"
        _ -> nodeb (addAttr <$> attrs) (toSvgBuilder <$> children)
    where
        nodeb as [] =
            "<" <> B.fromText name <> mconcat as <> "/>\n"
        nodeb as cs =
            "<" <> B.fromText name <> mconcat as <> ">\n"
                <> mconcat cs <>
            "</" <> B.fromText name <> ">\n"
        addAttr (n, v) =
            " " <> B.fromText n <> "=\"" <> escapeHtml v <> "\""
toSvgBuilder (SvgText txt) =
    escapeHtml txt

escapeHtml :: T.Text -> B.Builder
escapeHtml =
    let convert '<'  = "&lt;"
        convert '&'  = "&amp;"
        convert '>'  = "&gt;"
        convert '\"' = "&quot;"
        convert '\'' = "&apos;"
        convert c    = B.singleton c
    in T.foldr (\ c b -> convert c <> b) mempty

-------------------------------------------------------------

data SvgTree
    = SvgTree T.Text [SvgAttr] [SvgTree]
    | SvgText T.Text
    deriving (Eq, Show)

type SvgAttr = (T.Text, T.Text)

tree :: T.Text -> [SvgAttr] -> [SvgTree] -> SvgTree
tree = SvgTree

tree0 :: T.Text -> [SvgAttr] -> SvgTree
tree0 n as = SvgTree n as []

text :: T.Text -> SvgTree
text = SvgText

att :: T.Text -> T.Text -> SvgAttr
att = (,)


unitSize :: Double
unitSize = 20.0 -- in pixels

-- 'viewXMax' and 'viewYMax' are in pixels
renderSvgTree :: Int -> Int -> Drawing -> SvgTree
renderSvgTree viewXMax viewYMax draw =
    let vXM2 = fromInt (viewXMax*2)
        vYM2 = fromInt (viewYMax*2)
        nvXM = fromInt (-viewXMax)
        nvYM = fromInt (-viewYMax)
    in
    tree "svg" [ att "width" vXM2, att "height" vYM2, att "viewBox" (nvXM <> " " <> nvYM <> " " <> vXM2 <> " " <> vYM2)
               , att "xmlns" "http://www.w3.org/2000/svg", att "xmlns:xlink" "http://www.w3.org/1999/xlink"
               , att "buffered-rendering" "dynamic" ]
      [ tree "g" [ att "font-size" "14px", att "pointer-events" "none" ]
          ( tree0 "rect" [ att "vector-effect" "non-scaling-stroke"
                         , att "fill" "none", att "stroke" "gray", att "stroke-width" "2"
                         , att "x" nvXM, att "y" nvYM, att "width" vXM2, att "height" vYM2 ]
          : pprDrawing viewXMax viewYMax mempty (RGB 0 0 0) draw
          )
      ]

-- 'fromInt' and 'fromDouble' seem more efficient than 'Tl.decimal' and 'Tl.realFloat'.
fromInt :: Int -> T.Text
fromInt x = T.pack $ show x

fromDouble :: Double -> T.Text
fromDouble x =
    let (n, f) = properFraction x
    in if f == 0 then fromInt n
       else T.pack $ show x

pprDrawing :: Int -> Int -> Transform -> Color -> Drawing -> [SvgTree]
pprDrawing _ _ tr color (Fill shape) =
    pprShape tr (pprFill color) shape
pprDrawing _ _ tr color (Outline shape) =
    pprShape tr (pprOutline color) shape
pprDrawing _ _ tr _ (Image href w h) =
    let x' = (-w/2)*unitSize; y' = (-h/2)*unitSize; w' = w*unitSize; h' = h*unitSize
    in [ tree0 "image"
            (pprTransform tr <> [ att "href" href
                                , att "x" (fromDouble x'), att "y" (fromDouble y')
                                , att "width" (fromDouble w'), att "height" (fromDouble h') ])
       ]
pprDrawing viewXMax viewYMax tr color (Transformed tr2 draw) =
    pprDrawing viewXMax viewYMax (tr <> tr2) color draw
pprDrawing viewXMax viewYMax tr _ (Colored color2 draw) =
    pprDrawing viewXMax viewYMax tr color2 draw
pprDrawing _ _ _ _ Blank =
    mempty
pprDrawing viewXMax viewYMax tr color (Many draws) =
    mconcat (pprDrawing viewXMax viewYMax tr color <$> draws [])
pprDrawing viewXMax viewYMax tr _ CoordinatePlane =
    [ tree "g" (pprTransform tr) $
        [ tree0 "line" [ att "vector-effect" "non-scaling-stroke", att "stroke" "gray", att "stroke-width" "2"
                       , att "x1" ("-" <> fromInt viewXMax), att "y1" "0", att "x2" (fromInt viewXMax), att "y2" "0" ]
        , tree0 "line" [ att "vector-effect" "non-scaling-stroke", att "stroke" "gray", att "stroke-width" "2"
                       , att "x1" "0", att "y1" ("-" <> fromInt viewYMax), att "x2" "0", att "y2" (fromInt viewYMax) ]
        ]
        <> mconcat (hlines <$> [1 .. round (viewYMax' / unitSize)])
        <> mconcat (vlines <$> [1 .. round (viewXMax' / unitSize)])
    ]
    where
        viewXMax' = fromIntegral viewXMax
        viewYMax' = fromIntegral viewYMax
        hlines, vlines :: Int -> [SvgTree]
        hlines n =
          let c = fromIntegral n * unitSize
          in [ line (-viewXMax') c    viewXMax' c       -- horitzontal line at y = -n
             , line (-viewXMax') (-c) viewXMax' (-c)    -- horitzontal line at y = n
             , number "end" 0 c    (-n)
             , number "end" 0 (-c) n
             ]
        vlines n =
          let c = fromIntegral n * unitSize
          in [ line c (-viewYMax')    c viewYMax'       -- vertical line at x = n
             , line (-c) (-viewYMax') (-c) viewYMax'    -- vertical line at x = -n
             , number "middle" c    (unitSize*0.5) n
             , number "middle" (-c) (unitSize*0.5) (-n)
             ]
        line x1 y1 x2 y2 =
            tree0 "line" [ att "vector-effect" "non-scaling-stroke", att "stroke" "gray", att "stroke-width" "1"
                         , att "x1" (fromDouble x1), att "y1" (fromDouble y1)
                         , att "x2" (fromDouble x2), att "y2" (fromDouble y2) ]
        number anchor x y n =
            tree "text" [ att "fill" "gray", att "text-anchor" anchor
                        , att "transform" ("translate(" <> fromDouble x <> "," <> fromDouble y <> ") scale(0.7,0.7)")
                        , att "x" "0", att "y" "0" ]
              [ text (fromInt n) ]

pprFill :: Color -> [SvgAttr]
pprFill c =
    [ att "fill" (pprColor c) ]

pprOutline :: Color -> [SvgAttr]
pprOutline c =
    [ att "fill" "none"
    , att "vector-effect" "non-scaling-stroke"
    , att "stroke" (pprColor c)
    , att "stroke-width" "1" ]

pprColor :: Color -> T.Text
pprColor (RGB r g b) =
    "rgb(" <> fromComp r <> "," <> fromComp g <> "," <> fromComp b <> ")"
    where
        fromComp n = fromInt $ round $ n * 255

pprShape :: Transform -> [SvgAttr] -> Shape -> [SvgTree]
pprShape tr attrs (Path closed ps) =
    pprShape' 0 0 attrs (Path closed (transformPoint tr <$> ps))
pprShape tr attrs sh =
    case isTranslation tr of
        Just (tx, ty) ->
            pprShape' tx ty attrs sh
        Nothing ->
            pprShape' 0 0 (pprTransform tr <> attrs) sh

pprTransform :: Transform -> [SvgAttr]
pprTransform tr | isEmpty tr =
    []
pprTransform (Transform m11 m12 m21 m22 m31 m32) =
    [ att "transform" $
        "matrix(" <> fromDouble m11 <> "," <> fromDouble (-m12) <> ","
                  <> fromDouble (-m21) <> "," <> fromDouble m22 <> ","
                  <> fromDouble (m31*unitSize) <> "," <> fromDouble (-m32*unitSize) <> ")"
    ]
             {--     [ u  0 0 ]      H x Tapp = Tsvg x H
                 H = [ 0 -u 0 ]      Tsvg = H x Tapp x H^(-1)
                     [ 0  0 1 ]
                          [ 1/u  0  0 ]
                 H^(-1) = [  0 -1/u 0 ]
                          [  0   0  1 ]

                            [ m11 m21 m31 ]            [ m11*u  m21*u  m31*u  ]            [ m11  -m21 m31*u  ]
                 Tsvg = H x [ m12 m22 m32 ] x H^(-1) = [ -m12*u -m22*u -m32*u ] x H^(-1) = [ -m12 m22  -m32*u ]
                            [ 0   0   1   ]            [ 0      0      1      ]            [ 0    0    1      ]
             --}

pprShape' :: Double -> Double -> [SvgAttr] -> Shape -> [SvgTree]
pprShape' x0 y0 attrs (Rect w h) =
    let x' = (x0-w/2)*unitSize; y' = (-y0-h/2)*unitSize; w' = w*unitSize; h' = h*unitSize
    in [ tree0 "rect" (attrs <> [ att "x" (fromDouble x'), att "y" (fromDouble y')
                                , att "width" (fromDouble w'), att "height" (fromDouble h') ])
       ]
pprShape' x0 y0 attrs (Ellipse rx ry) =
    let x' = x0*unitSize; y' = -y0*unitSize; rx' = rx*unitSize; ry' = ry*unitSize
    in [ tree0 "ellipse" (attrs <> [ att "cx" (fromDouble x'), att "cy" (fromDouble y')
                                   , att "rx" (fromDouble rx'), att "ry" (fromDouble ry') ])
       ]
pprShape' _  _  _     (Path _closed []   ) = mempty
pprShape' x0 y0 attrs (Path closed (p:ps)) =
    [ tree0 "path" (attrs <> [ att "d" ("M" <> point p <> go ps) ]) ]
    where
        point (x, y) =
            let x' = (x0+x)*unitSize; y' = -(y0+y)*unitSize
            in fromDouble x' <> " " <> fromDouble y'
        go (p':ps') = " L" <> point p' <> go ps'
        go []       = if closed then " Z" else ""
pprShape' x0 y0 attrs (TextS anchor s) =
    let x' = x0*unitSize; y' = -y0*unitSize
        anchorText StartAnchor = "start"
        anchorText MiddleAnchor = "middle"
        anchorText EndAnchor = "end"
    in [ tree "text" (attrs <> [ att "text-anchor" (anchorText anchor)
                               , att "x" (fromDouble x'), att "y" (fromDouble y') ])
           [ text s ]
       ]

