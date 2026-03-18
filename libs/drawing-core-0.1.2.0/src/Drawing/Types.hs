
{-# LANGUAGE OverloadedStrings #-}

module Drawing.Types where
import Data.Aeson
import Data.Aeson.Encoding.Internal
import Data.Text (Text)


data Drawing =
      Fill Shape
    | Outline Shape
    | Image Text Double Double        -- href, width, height
    | CoordinatePlane
    | Transformed Transform Drawing
    | Colored Color Drawing
    | Blank
    | Many ([Drawing] -> [Drawing])     -- diff lists

instance Eq Drawing where
    Fill s1             == Fill s2             = s1 == s2
    Outline s1          == Outline s2          = s1 == s2
    Image u1 w1 h1      == Image u2 w2 h2      = u1 == u2 && w1 == w2 && h1 == h2
    CoordinatePlane     == CoordinatePlane     = True
    Transformed tr1 dr1 == Transformed tr2 dr2 = tr1 == tr2 && dr1 == dr2
    Colored c1 dr1      == Colored c2 dr2      = c1 == c2 && dr1 == dr2
    Blank               == Blank               = True
    Many dl1            == Many dl2            = dl1 [] == dl2 []
    _                   == _                   = False

instance Show Drawing where
    showsPrec p (Fill s) =
      showParen (p > 10) $
        showString "Fill " . showsPrec 11 s
    showsPrec p (Outline s) =
      showParen (p > 10) $
        showString "Outline " . showsPrec 11 s
    showsPrec p (Image u w h) =
      showParen (p > 10) $
        showString "Image " . shows u . showChar ' '  . shows w . showChar ' ' . shows h
    showsPrec _ CoordinatePlane =
      showString "CoordinatePlane"
    showsPrec p (Transformed tr dr) =
      showParen (p > 10) $
        showString "Transformed " . showsPrec 11 tr . showChar ' ' . showsPrec 11 dr
    showsPrec p (Colored c dr) =
      showParen (p > 10) $
        showString "Colored " . showsPrec 11 c . showChar ' ' . showsPrec 11 dr
    showsPrec _ Blank =
      showString "Blank"
    showsPrec p (Many dl) =
      showParen (p > 10) $
        showString "Many " . showsPrec 11 (dl [])

-- | 'Drawing's are 'Semigroup's. The composing function is the overlapping
-- (the right drawing is drawn over the left one).
instance Semigroup Drawing where
    Blank <> d2 =
        d2
    d1 <> Blank =
        d1
    (Many f1) <> (Many f2) =
        Many (f1 . f2)
    (Many f1) <> d2 =
        Many (\ds -> f1 (d2 : ds))
    d1 <> (Many f2) =
        Many (\ds -> d1 : f2 ds)
    d1 <> d2 =
        Many (\ds -> d1 : d2 : ds)

-- | 'Drawing's are 'Monoid's. The identity is a 'blank' (completely transparent)
-- drawing and the composing function is the overlapping (the right drawing is
-- drawn over the left one).
instance Monoid Drawing where
    mempty = Blank
    mappend = (<>)


-- | A point on the x-y plane.
type Point = (Double, Double)

data Shape =
      Rect Double Double        -- width, height
    | Path Bool [Point]
    | Ellipse Double Double     -- X-radius, Y-radius
    | TextS TextAnchor Text
    deriving (Eq, Show)


-----------------------------------------------------------------------------

data Color = RGB !Double !Double !Double
    deriving (Eq, Show)

-- Utility functions.
fence :: Double -> Double
fence = max 0 . min 1

wrapNum :: Double -> Double -> Double
wrapNum lim x = x - fromInteger (floor (x / lim)) * lim


fromRGB :: Double -> Double -> Double -> Color
fromRGB r' g' b' = RGB (fence r') (fence g') (fence b')

-- Based on the algorithm from the CSS3 specification.
fromHSL :: Double -> Double -> Double -> Color
fromHSL h' s' l' = RGB r g b
  where
    h = wrapNum (2 * pi) h'
    s = fence s'
    l = fence l'
    m1 = l * 2 - m2
    m2
      | l <= 0.5 = l * (s + 1)
      | otherwise = l + s - l * s
    r = convert (h / 2 / pi + 1 / 3)
    g = convert (h / 2 / pi)
    b = convert (h / 2 / pi - 1 / 3)
    convert hn
      | hn < 0 = convert (hn + 1)
      | hn > 1 = convert (hn - 1)
      | hn * 6 < 1 = m1 + (m2 - m1) * hn * 6
      | hn * 2 < 1 = m2
      | hn * 3 < 2 = m1 + (m2 - m1) * (2 / 3 - hn) * 6
      | otherwise = m1

toRGB :: Color -> (Double, Double, Double)
toRGB (RGB r g b) = (r, g, b)

toHSL :: Color -> (Double, Double, Double)
toHSL c = (hue c, saturation c, luminosity c)

hue :: Color -> Double
hue (RGB r g b)
  | hi - lo < epsilon = 0
  | r == hi && g >= b = (g - b) / (hi - lo) * pi / 3
  | r == hi = (g - b) / (hi - lo) * pi / 3 + 2 * pi
  | g == hi = (b - r) / (hi - lo) * pi / 3 + 2 / 3 * pi
  | otherwise = (r - g) / (hi - lo) * pi / 3 + 4 / 3 * pi
  where
    hi = max r (max g b)
    lo = min r (min g b)
    epsilon = 0.000001

saturation :: Color -> Double
saturation (RGB r g b)
  | hi - lo < epsilon = 0
  | otherwise = (hi - lo) / (1 - abs (hi + lo - 1))
  where
    hi = max r (max g b)
    lo = min r (min g b)
    epsilon = 0.000001

luminosity :: Color -> Double
luminosity (RGB r g b) = (lo + hi) / 2
  where
    hi = max r (max g b)
    lo = min r (min g b)

lighter :: Double -> Color -> Color
lighter d c =
  fromHSL (hue c) (saturation c) (fence (luminosity c + d))

light :: Color -> Color
light = lighter 0.15

dark :: Color -> Color
dark = lighter (-0.15)

brighter :: Double -> Color -> Color
brighter d c =
  fromHSL (hue c) (fence (saturation c + d)) (luminosity c)

bright :: Color -> Color
bright = brighter 0.25

dull :: Color -> Color
dull = brighter (-0.25)


-----------------------------------------------------------------------------

type FillStyle = Color

type OutlineStyle = Color

data TextAnchor = StartAnchor | MiddleAnchor | EndAnchor
    deriving (Eq, Show)


-----------------------------------------------------------------------------
-- | An affine transformation.
--
-- An affine transformation is any transformation that preserves collinearity (i.e., all points lying on a line initially
-- still lie on a line after transformation) and ratios of distances (e.g., the midpoint of a line segment remains
-- the midpoint after transformation). An affine transformation is also called an affinity.
data Transform = Transform { _tr11 :: Double, _tr12 :: Double, _tr21 :: Double, _tr22 :: Double, _tr31 :: Double, _tr32 :: Double}
    deriving Eq

-- | 'Transform's are 'Semigroup's. `<>` is
-- the composition of transformations.
instance Semigroup Transform where
    Transform a11 a12 a21 a22 a31 a32 <> Transform b11 b12 b21 b22 b31 b32 =
        {- [a11 a21 a31]   [b11 b21 b31]   [a11*b11+a21*b12 a11*b21+a21*b22 a11*b31+a21*b32+a31]
         - [a12 a22 a32] x [b12 b22 b32] = [a12*b11+a22*b12 a12*b21+a22*b22 a12*b31+a22*b32+a32]
         - [0   0   1  ]   [0   0   1  ]   [0               0               1                  ]
         -}
        Transform { _tr11 = a11*b11+a21*b12, _tr21 = a11*b21+a21*b22, _tr31 = a11*b31+a21*b32+a31,
                    _tr12 = a12*b11+a22*b12, _tr22 = a12*b21+a22*b22, _tr32 = a12*b31+a22*b32+a32 }

-- | 'Transform's are 'Monoid's. `mempty` is the identity transformation
-- (e.g.: @translation 0 0@, or @rotation 0@, ...) and `mappend` is
-- the composition of transformations.
instance Monoid Transform where
    mempty =
        Transform { _tr11 = 1.0, _tr12 = 0.0, _tr21 = 0.0, _tr22 = 1.0, _tr31 = 0.0, _tr32 = 0.0 }
    mappend = (<>)

instance Show Transform where
    show (Transform m11 m12 m21 m22 m31 m32) =
        "Transform " <> show m11 <> " " <> show m12 <> " " <> show m21 <> " " <> show m22 <> " " <> show m31 <> " " <> show m32

-- | A translation by the given distance vector.
translation :: (Double, Double) -> Transform
translation (dx, dy) =
    Transform { _tr11 = 1.0, _tr12 = 0.0, _tr21 = 0.0, _tr22 = 1.0, _tr31 = dx, _tr32 = dy }

-- | A scaling by the given x and y factors. Scaling
-- by a negative factor also reflects across that axis.
scaling :: Double -> Double -> Transform
scaling sx sy =
    Transform { _tr11 = sx, _tr12 = 0.0, _tr21 = 0.0, _tr22 = sy, _tr31 = 0.0, _tr32 = 0.0 }

-- | An uniformly scaling by the given factor.
-- Dilating by a negative factor also reflects across the origin.
--
-- prop> dilated s = scaled s s
dilation :: Double -> Transform
dilation s = scaling s s

-- | A rotation by the given angle about the origin.
-- Angles are in radians.
rotation :: Double -> Transform
rotation a =
    Transform { _tr11 = cos a, _tr12 = sin a, _tr21 = -sin a, _tr22 = cos a, _tr31 = 0.0, _tr32 = 0.0 }

{-
[a b c]   [1 t 0]   [a t*a+b c]
[d e f] x [0 1 0] = [d t*d+e f]
[0 0 1]   [0 0 1]   [0 0     1]
-}
skewX :: Double -> Transform
skewX a =
    Transform { _tr11 = 1.0, _tr12 = 0.0, _tr21 = tan a, _tr22 = 1.0, _tr31 = 0.0, _tr32 = 0.0 }

{-
[a b c]   [1 0 0]   [a+t*b b c]
[d e f] x [t 1 0] = [d+t*e e f]
[0 0 1]   [0 0 1]   [0     0 1]
-}
skewY :: Double -> Transform
skewY a =
    Transform { _tr11 = 1.0, _tr12 = tan a, _tr21 = 0.0, _tr22 = 1.0, _tr31 = 0.0, _tr32 = 0.0 }

isEmpty :: Transform -> Bool
isEmpty =
    maybe False ((0, 0) ==) . isTranslation

isTranslation :: Transform -> Maybe (Double, Double)
isTranslation (Transform m11 m12 m21 m22 m31 m32) =
    if m11 == 1 && m12 == 0 && m21 == 0 && m22 == 1 then Just (m31, m32)
    else Nothing


-- | Apply an affine transformation to a drawing.
transform :: Transform -> Drawing -> Drawing
{- NOTE: Previous implementation did special treatment for cases:
 - But it seems more efficient to maintain the transformation matrix and generate better SVG code.
transform tr (Many drs)           = Many (transform tr <$> drs)
transform tr (Fill (Path closed ps) style)    = Fill (Path closed (transformPoint tr <$> ps)) style
transform tr (Outline (Path closed ps) style) = Outline (Path closed (transformPoint tr <$> ps)) style
 -}
transform tr (Transformed tr2 dr) =
    transform (tr <> tr2) dr
transform tr dr                   =
    if isEmpty tr then dr else Transformed tr dr

-- | Apply an affine transformation to a point.
transformPoint :: Transform -> Point -> Point
transformPoint (Transform a11 a12 a21 a22 a31 a32) (x, y) =
    (a11*x + a21*y + a31, a12*x + a22*y + a32)

-- | Apply the linear part of an affine transformation (that's a linear tranformation,
-- ignoring the translation part) to a vector.
transformVector :: Transform -> (Double, Double) -> (Double, Double)
transformVector (Transform a11 a12 a21 a22 _ _) (dx, dy) =
    (a11*dx + a21*dy, a12*dx + a22*dy)

--------------------------------------------------------------------
-- ToJSON instances

instance ToJSON Drawing where
    toEncoding (Fill sh) =
        tuple (toEncoding ("Fll"::String) >*< toEncoding sh)
    toEncoding (Outline sh) =
        tuple (toEncoding ("Otl"::String) >*< toEncoding sh)
    toEncoding (Image href width height) =
        tuple (toEncoding ("Img"::String) >*< toEncoding href >*< toEncoding width >*< toEncoding height)
    toEncoding CoordinatePlane =
        wrapArray (toEncoding ("Crd"::String))
    toEncoding (Transformed tr d) =
        tuple (toEncoding ("Trn"::String) >*< toEncoding tr >*< toEncoding d)
    toEncoding (Colored co d) =
        tuple (toEncoding ("Col"::String) >*< toEncoding co >*< toEncoding d)
    toEncoding Blank =
        wrapArray (toEncoding ("Bln"::String))
    toEncoding (Many f) =
        tuple (toEncoding ("Mny"::String) >*< toEncoding (f []))
    toJSON _ = error "Drawing.toJSON: Not yet implemented"

instance ToJSON Shape where
    toEncoding (Rect w h) =
        tuple (toEncoding ("Rct"::String) >*< toEncoding w >*< toEncoding h)
    toEncoding (Path closed ps) =
        tuple (toEncoding ("Pth"::String) >*< toEncoding closed >*< toEncoding ps)
    toEncoding (Ellipse rx ry) =
        tuple (toEncoding ("Ell"::String) >*< toEncoding rx >*< toEncoding ry)
    toEncoding (TextS anchor t) =
        tuple (toEncoding ("Txt"::String) >*< toEncoding anchor >*< toEncoding t)
    toJSON _ = error "Shape.toJSON: Not yet implemented"

instance ToJSON Color where
    toEncoding (RGB r g b) =    -- encode to a CSS string
        toEncoding $ "rgb(" <> to255 r <> "," <> to255 g <> "," <> to255 b <> ")"
        where
            to255 n = show $ (round $ n * 255 :: Int)
    toJSON _ = error "Color.toJSON: Not yet implemented"

instance ToJSON Transform where
    toEncoding (Transform m11 m12 m21 m22 m31 m32) =
        toEncoding [m11, m12, m21, m22, m31, m32]
    toJSON _ = error "Transform.toJSON: Not yet implemented"

instance ToJSON TextAnchor where
    toEncoding StartAnchor =
        toEncoding ("Sta"::String)
    toEncoding MiddleAnchor =
        toEncoding ("Mid"::String)
    toEncoding EndAnchor =
        toEncoding ("End"::String)
    toJSON _ = error "TextAnchor.toJSON: Not yet implemented"

