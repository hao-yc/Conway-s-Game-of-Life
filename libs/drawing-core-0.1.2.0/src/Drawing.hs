{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Drawing
    (
    -- * Types
      Drawing, Point, Color(..), TextAnchor(..)
    -- * Basic drawings
    , blank, polyline, polygon, solidPolygon, rectangle, solidRectangle, circle, solidCircle
    , text, atext, image, coordinatePlane
    -- * Modification of drawings
    , colored, translated, scaled, dilated, rotated, xSkewed, ySkewed
    -- * Colors
    , white, black, gray, red, green, lime, blue, yellow, orange, brown
    , fromRGB, fromHSL, toRGB, toHSL
    , hue, saturation, luminosity
    , lighter, light, dark
    , brighter, bright, dull
    -- * Text anchors
    , startAnchor, middleAnchor, endAnchor
    -- * Composition
    , (<>)
    -- * Affine transformations
    , Transform
    , translation, scaling, dilation, rotation, skewX, skewY
    , transform, transformPoint, transformVector
    -- * I/O and interaction
    , module Drawing.IO
    )
where
import Drawing.Types
import Drawing.IO

import Data.Text (Text)
import Data.Foldable

-- * Basic drawings

-- | A blank drawing, with nothing in it.
blank :: Drawing
blank = Blank

-- | A sequence of line segments, with given endpoints.
polyline :: forall f. Foldable f => f Point -> Drawing
polyline ps = Outline (Path False $ toList ps)

-- | A thin polygon with these points as vertices.
polygon :: forall f. Foldable f => f Point -> Drawing
polygon ps = Outline (Path True $ toList ps)

-- | A solid polygon with these points as vertices.
solidPolygon :: forall f. Foldable f => f Point -> Drawing
solidPolygon ps = Fill (Path True $ toList ps)

rectangle :: Double -> Double -> Drawing
rectangle w h = Outline (Rect w h)

solidRectangle :: Double -> Double -> Drawing
solidRectangle w h = Fill (Rect w h)

circle :: Double -> Drawing
circle r = Outline (Ellipse r r)

solidCircle :: Double -> Drawing
solidCircle r = Fill (Ellipse r r)


-- |
-- prop> text s = atext middleAnchor s
text :: Text -> Drawing
text s = atext middleAnchor s

atext :: TextAnchor -> Text -> Drawing
atext anchor s = Fill (TextS anchor s)


image :: Text -> Double -> Double -> Drawing
image uri w h = Image uri w h


-- | A coordinate plane.  Adding this to your pictures can help you measure distances
-- more accurately.
--
-- Example:
--
-- > main = putSvg (coordinatePlane <> myPicture)
-- > myPicture = ...
--
coordinatePlane :: Drawing
coordinatePlane = CoordinatePlane


-- * Modification of drawings

-- | A drawing drawn with this color.
colored :: Color -> Drawing -> Drawing
colored = Colored


-- | A drawing translated by the given x and y coordinates.
--
-- prop> translated dx dy = transform (translation (dx, dy))
translated :: Double -> Double -> Drawing -> Drawing
translated dx dy = transform (translation (dx, dy))

-- | A drawing scaled by the given x and y factors.  Scaling
-- by a negative factor also reflects across that axis.
--
-- prop> scaled sx sy = transform (scaling sx sy)
scaled :: Double -> Double -> Drawing -> Drawing
scaled sx sy = transform (scaling sx sy)

-- | A drawing scaled uniformly by the given factor.
-- Dilating by a negative factor also reflects across the origin.
--
-- prop> dilated s = scaled s s
dilated :: Double -> Drawing -> Drawing
dilated s = transform (scaling s s)

-- | A drawing rotated by this angle about the origin.
--
-- Angles are in radians.
--
-- prop> rotated a = transform (rotation a)
rotated :: Double -> Drawing -> Drawing
rotated a = transform (rotation a)

-- |
-- prop> xSkewed a = transform (skewX a)
xSkewed :: Double -> Drawing -> Drawing
xSkewed a = transform (skewX a)

-- |
-- prop> ySkewed a = transform (skewY a)
ySkewed :: Double -> Drawing -> Drawing
ySkewed a = transform (skewY a)


-- * Colors

white, black, gray, red, green, lime, blue, yellow, orange, brown :: Color
white = RGB 1 1 1
black = RGB 0 0 0

gray = RGB 0.5 0.5 0.5

red = RGB 1 0 0
green = RGB 0 0.5 0
lime = RGB 0 1 0
blue = RGB 0 0 1

yellow = RGB 1 1 0
orange = RGB 1 0.64 0
brown = RGB 0.64 0.16 0.16


-- * Text anchors

startAnchor, middleAnchor, endAnchor :: TextAnchor
startAnchor = StartAnchor
middleAnchor = MiddleAnchor
endAnchor = EndAnchor

