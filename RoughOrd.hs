module RoughOrd  (RoughOrd (..))
    where
{- Shame on me,
   numbers are compared just to each other (and to 0) 
   without an external reference.
   Can you tell, if 0.00011 and 0.00012 are close enough to each other ? -}

-- | Mnemonics for the signs are their mathematical names:
--   approximately greater,
--   approximately greater or equal, etc 
class (Ord a, Fractional a) => RoughOrd a
    where 
     roughlyEqual, (=~=), (~>), (~<), (~>=), (~<=) :: a -> a -> Bool
     (=~=) = roughlyEqual
     x ~> y = not (x ~<= y)
     x ~>= y = (x > y) || (x =~= y)
     x ~< y = not (x~>=y)
     x ~<= y = (x < y) || (x =~= y)
     roughEqualityPrecision :: a
     roughlyEqual x y = abs (x-y) <= roughEqualityPrecision * (abs x + abs y)
     -- the case when x=0 and/or y=0 must be treated correctly
instance RoughOrd Double  
    where
     roughEqualityPrecision = 0.001 -- was 0.01 just by an order of magnitude

