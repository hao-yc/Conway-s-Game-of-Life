
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Drawing.Vector where
import Data.Kind (Type)


infixl 6 ^+^, ^-^

-- | Additive group @v@.
class AdditiveGroup v where
  -- | The zero element: identity for '(^+^)'
  zeroV :: v
  -- | Add vectors
  (^+^) :: v -> v -> v
  -- | Additive inverse
  negateV :: v -> v
  -- | Group subtraction
  (^-^) :: v -> v -> v
  v ^-^ v' = v ^+^ negateV v'

-- | Sum over several vectors
sumV :: (Foldable f, AdditiveGroup v) => f v -> v
sumV = foldr (^+^) zeroV
{-# INLINE sumV #-}

------------------------------------------------------------------

infixr 7 *^

-- | Vector space @v@.
class AdditiveGroup v => VectorSpace v where
  type Scalar v :: Type
  -- | Scale a vector
  (*^) :: Scalar v -> v -> v

-- | Vector divided by scalar
(^/) :: (VectorSpace v, s ~ Scalar v, Fractional s) => v -> s -> v
v ^/ s = recip s *^ v
{-# INLINE (^/) #-}

-- | Vector multiplied by scalar
(^*) :: (VectorSpace v, s ~ Scalar v) => v -> s -> v
(^*) = flip (*^)
{-# INLINE (^*) #-}

------------------------------------------------------------------

infixr 7 <.>

-- | Inner product space @v@.
class (VectorSpace v, AdditiveGroup (Scalar v)) => InnerSpace v where
  -- | Inner/dot product
  (<.>) :: v -> v -> Scalar v

-- | Square of the length of a vector.  Sometimes useful for efficiency.
-- See also 'magnitude'.
magnitudeSq :: (InnerSpace v, s ~ Scalar v) => v -> s
magnitudeSq v = v <.> v
{-# INLINE magnitudeSq #-}

-- | Length of a vector.   See also 'magnitudeSq'.
magnitude :: (InnerSpace v, s ~ Scalar v, Floating s) =>  v -> s
magnitude = sqrt . magnitudeSq
{-# INLINE magnitude #-}

-- | Vector in same direction as given one but with length of one.  If
-- given the zero vector, then return it.
normalized :: (InnerSpace v, s ~ Scalar v, Floating s) =>  v -> v
normalized v = v ^/ magnitude v
{-# INLINE normalized #-}

------------------------------------------------------------------
-- Instances

instance AdditiveGroup Int where
    zeroV = 0
    x1 ^+^ x2 = x1 + x2
    negateV x = -x
    x1 ^-^ x2 = x1 - x2

-- but Int is not an instance of VectorSpace

instance AdditiveGroup Double where
    zeroV = 0
    x1 ^+^ x2 = x1 + x2
    negateV x = -x
    x1 ^-^ x2 = x1 - x2

instance VectorSpace Double where
    type Scalar Double = Double
    s *^ x = s * x

instance InnerSpace Double where
    (<.>) = (*)


instance (AdditiveGroup u, AdditiveGroup v) => AdditiveGroup (u, v) where
    zeroV = (zeroV, zeroV)
    (x1, y1) ^+^ (x2, y2) = (x1 ^+^ x2, y1 ^+^ y2)
    negateV (x, y) = (negateV x, negateV y)

instance ( VectorSpace u, s ~ Scalar u
         , VectorSpace v, s ~ Scalar v ) => VectorSpace (u, v) where
    type Scalar (u, v) = Scalar u
    s *^ (x, y) = (s *^ x, s *^ y)

instance ( InnerSpace u, s ~ Scalar u
         , InnerSpace v, s ~ Scalar v ) => InnerSpace (u, v) where
    (x, y) <.> (x', y') = (x <.> x') ^+^ (y <.> y')


instance (AdditiveGroup u, AdditiveGroup v, AdditiveGroup w) => AdditiveGroup (u, v, w) where
    zeroV = (zeroV, zeroV, zeroV)
    (u, v, w) ^+^ (u', v', w') = (u ^+^ u', v ^+^ v', w ^+^ w')
    negateV (u, v, w) = (negateV u, negateV v, negateV w)

instance ( VectorSpace u, s ~ Scalar u
         , VectorSpace v, s ~ Scalar v
         , VectorSpace w, s ~ Scalar w ) => VectorSpace (u, v, w) where
    type Scalar (u, v, w) = Scalar u
    s *^ (u, v, w) = (s *^ u, s *^ v, s *^ w)

instance ( InnerSpace u, s ~ Scalar u
         , InnerSpace v, s ~ Scalar v
         , InnerSpace w, s ~ Scalar w ) => InnerSpace (u, v, w) where
    (u, v, w) <.> (u', v', w') = (u <.> u') ^+^ (v <.> v') ^+^ (w <.> w')


instance AdditiveGroup a => AdditiveGroup [a] where
    zeroV = repeat zeroV
    xs1 ^+^ xs2 = zipWith (^+^) xs1 xs2
    negateV xs = map negateV xs

instance VectorSpace a => VectorSpace [a] where
    type Scalar [a] = Scalar a
    s *^ xs = map (s *^) xs

instance InnerSpace a => InnerSpace [a] where
    xs <.> xs' = sumV $ zipWith (<.>) xs xs'

