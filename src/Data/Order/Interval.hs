{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE Safe              #-}

module Data.Order.Interval (
    Interval()
  , imap
  , (...)
  , iempty
  , singleton
  , contains
  , endpts
  , above
  , below
  , interval
) where

import safe Control.Applicative (liftA2)
import safe Data.Connection.Conn
import safe Data.Bifunctor (bimap)
import safe Data.Lattice
import safe Data.Order
import safe Data.Order.Syntax
import safe Prelude hiding (Ord(..), Eq(..), Bounded, until)
import safe qualified Data.Eq as Eq

---------------------------------------------------------------------
-- Intervals
---------------------------------------------------------------------

-- | An interval in a poset /P/.
--
-- An interval in a poset /P/ is a subset /I/ of /P/ with the following property:
--
-- \( \forall x, y \in I, z \in P: x \leq z \leq y \Rightarrow z \in I \)
--
data Interval a = Empty | I !a !a deriving Show

-- | Map over an interval.
--
-- /Note/ this is not a functor, as a non-monotonic map
-- may cause the interval to collapse to the iempty interval.
--
imap :: Preorder b => (a -> b) -> Interval a -> Interval b
imap f = maybe iempty (uncurry (...)) . fmap (bimap f f) . endpts

infix 3 ...

-- | Construct an interval from a pair of points.
--
-- /Note/: Endpoints are preorder-sorted. If /pcompare x y = Nothing/
-- then the resulting interval will be empty.
-- 
(...) :: Preorder a => a -> a -> Interval a
x ... y = case pcompare x y of
  Nothing -> Empty
  Just GT -> I y x
  otherwise -> I x y
{-# INLINE (...) #-}

-- | The iempty interval.
--
-- >>> iempty
-- Empty
--
iempty :: Interval a
iempty = Empty
{-# INLINE iempty #-}

-- | Construct an interval containing a single point.
--
-- >>> singleton 1
-- 1 ... 1
--
singleton :: a -> Interval a
singleton a = I a a
{-# INLINE singleton #-}

-- | Obtain the endpoints of an interval.
--
endpts :: Interval a -> Maybe (a, a)
endpts Empty = Nothing
endpts (I x y) = Just (x, y)
{-# INLINE endpts #-}

contains :: Preorder a => Interval a -> a -> Bool
contains Empty _ = False
contains (I x y) p = x <~ p && p <~ y

-- | \( X_\geq(x) = \{ y \in X | y \geq x \} \)
--
-- Construct the upper set of an element /x/.
--
-- This function is monotone:
--
-- > x <~ y <=> above x <~ above y
--
-- by the Yoneda lemma for preorders.
--
above :: UpperBounded a => a -> Interval a
above x = x ... top
{-# INLINE above #-}

-- | \( X_\leq(x) = \{ y \in X | y \leq x \} \)
--
-- Construct the lower set of an element /x/.
--
-- This function is antitone:
--
-- > x <~ y <=> below x >~ below y
--
below :: LowerBounded a => a -> Interval a
below x = bottom ... x
{-# INLINE below #-}

interval :: LowerBounded a => Conn k (Maybe a) (Interval a)
interval = trip f g h where
  f = maybe iempty singleton
  g = maybe Nothing (Just . uncurry (\/)) . endpts
  h = maybe iempty below

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

instance Eq a => Eq (Interval a) where
  Empty == Empty = True
  Empty == _ = False
  _ == Empty = False
  I x y == I x' y' = x == x' && y == y'

-- | A < https://en.wikipedia.org/wiki/Containment_order containment order >
--
instance Preorder a => Preorder (Interval a) where
  Empty <~ _ = True
  _ <~ Empty = False
  I x y <~ I x' y' = x' <~ x && y <~ y'

instance Lattice a => Semigroup (Join (Interval a)) where
  (<>) = liftA2 joinInterval

joinInterval Empty i = i
joinInterval i Empty = i
joinInterval (I x y) (I x' y') = I (x /\ x') (y \/ y')

instance Lattice a => Monoid (Join (Interval a)) where
  mempty = pure Empty
