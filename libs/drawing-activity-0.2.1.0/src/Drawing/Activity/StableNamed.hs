
{-# LANGUAGE CPP                        #-}

module Drawing.Activity.StableNamed
where
import           System.IO.Unsafe
import           System.Mem.StableName

---------------------------------
-- Object with a stable name (for fast equality)

data StableNamed a = StableNamed !a !(StableName a)

mkStableNamed :: a -> StableNamed a
mkStableNamed x = unsafePerformIO $ do
    sname <- x `seq` makeStableName x
    pure $ StableNamed x sname

unStableNamed :: StableNamed a -> a
unStableNamed (StableNamed x _) = x

stableName :: StableNamed a -> StableName a
stableName (StableNamed _ n) = n

#ifdef TEST_STABLENAMED
-- Test version:
instance Eq a => Eq (StableNamed a) where
    StableNamed x1 sn1 == StableNamed x2 sn2 = unsafePerformIO $ do
        when (sn1 /= sn2) $ do
            let isNeq = x1 /= x2
            putStrLn $
                if isNeq then "sn1 /= sn2 and x1 /= x2"
                         else "sn1 /= sn2 but x1 == x2 !!!"
        pure $ sn1 == sn2
#else
instance Eq (StableNamed a) where
    StableNamed _ sn1 == StableNamed _ sn2 =
        sn1 == sn2
#endif

-- | Get the  stable name and converts it to an 'Int'.  The 'Int' returned is not
-- necessarily unique; several stable names may map to the same 'Int'
-- (in practice however, the chances of this are small, so the result
-- of 'hashStableNamed' makes a good hash key).
hashStableNamed :: StableNamed a -> Int
hashStableNamed (StableNamed _ n) = hashStableName n

---------------------------------

type StableFun a b = StableNamed (a -> b)

