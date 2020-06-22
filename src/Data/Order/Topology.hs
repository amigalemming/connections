{-# Language TypeApplications    #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language ConstraintKinds     #-}
{-# Language DataKinds           #-}
{-# Language KindSignatures      #-}
{-# Language RankNTypes          #-}
{-# Language Safe                #-}

module Data.Order.Topology (
  -- * Left and right separated Alexandrov topologies
    Kan(..)
  , Open()
  , lower
  , upper
  , omap
  -- ** The left (/Inf/) topology
  , Inf
  , inf
  , (.?)
  , filterL
  -- ** The right (/Sup/) topology
  , Sup 
  , sup
  , (?.)
  , filterR
) where

import safe Control.Applicative (liftA2)
import safe Data.Connection.Conn
import safe Data.Semigroup.Join
import safe Data.Lattice
import safe Data.Order
import safe Data.Universe.Class (Finite(..))
import safe Prelude hiding (Bounded, Eq(..), Ord(..))

-- | A pointed open set in a separated left or right < https://en.wikipedia.org/wiki/Alexandrov_topology Alexandrov topology >.
--
data Open (k :: Kan) a = Open (a -> Bool) a a

-- | The base point of a pointed lower set (i.e. an 'Inf').
--
lower :: Open k a -> a
lower (Open _ x _) = x

-- | The base point of a pointed upper set (i.e. a 'Sup').
-- 
upper :: Open k a -> a
upper (Open _ _ y) = y

-- | Map over a pointed open set in either topology.
--
omap :: Trip a b -> Open k a -> Open k b
omap (Conn f g h) (Open p x y) = Open (p . g) (h x) (f y)

-- Up-set ideals
up :: Preorder a => a -> a -> Bool
up a = (a <~)

-- Up-set anti-ideals
up' :: Preorder a => a -> a -> Bool
up' a = fmap not (a <~)

-- Down-set ideals
down :: Preorder a => a -> a -> Bool
down a = (<~ a)

-- Down-set anti-ideals
down' :: Preorder a => a -> a -> Bool
down' a = fmap not (<~ a)

---------------------------------------------------------------------
-- Inf
---------------------------------------------------------------------

type Inf = Open 'L


-- | Create an upper set: \( X_\geq(x) = \{ y \in X | y \geq x \} \)
--
-- Upper sets are the < https://en.wikipedia.org/wiki/Ideal_(order_theory) open sets > of the left Alexandrov topology.
--
-- This function is monotone:
--
-- > x <~ y <=> inf x <~ inf y
--
-- by the Yoneda lemma for preorders.
--
inf :: UpperBounded a => a -> Inf a
inf a = Open (const True) a top

infix 4 .?

-- | Check for membership in an /Inf/.
--
(.?) :: Preorder a => Inf a -> a -> Bool
(.?) (Open f l _) a = f a && down a l


infixr 5 `filterL`

-- | Filter an /Inf/ with an anti-filter.
--
-- The resulting set is open in the separated left Alexandrov topology.
--
-- Intersecting with an incomparable element cuts out everything
-- larger than its join with the base point:
--
-- >>> p = inf pi :: Inf Double
-- >>> p .? 1/0
-- True
-- >>> filterL (0/0) p .? 1/0
-- False
--
-- An example w/ the set inclusion lattice:
-- >>> x = Set.fromList [6,40] :: Set.Set Word8
-- >>> y = Set.fromList [1,6,9] :: Set.Set Word8
-- >>> z = filterL y $ inf x
-- fromList [6,40] ... fromList [1,6,9,40]
-- >>> z .? Set.fromList [1,6,40]
-- True
-- >>> z .? Set.fromList [6,9,40]
-- True
-- >>> z .? Set.fromList [1,6,9,40]
-- False
--
filterL :: Lattice a => a -> Inf a -> Inf a
filterL a p@(Open f l u) = if down' l a then Open (f /\ up' a) l (glb l a u) else p

---------------------------------------------------------------------
-- Sup
---------------------------------------------------------------------

type Sup = Open 'R

-- | Create a lower set \( X_\leq(x) = \{ y \in X | y \leq x \} \)
--
-- Lower sets are the <https://en.wikipedia.org/wiki/Filter_(mathematics) open sets > of the right Alexandrov topology.
--
-- This function is antitone:
--
-- > x <~ y <=> sup x >~ sup y
--
-- by the Yoneda lemma for preorders.
--
sup :: LowerBounded a => a -> Sup a
sup a = Open (const True) bottom a

infix 4 ?.

-- | Check for membership in a /Sup/.
--
(?.) :: Preorder a => a -> Sup a -> Bool
(?.) a (Open f _ u) = f a && up a u

infixl 5 `filterR`

-- | Filter a /Sup/ with an anti-ideal.
-- 
-- The resulting set is open in the separated right Alexandrov topology.
--
-- >>> p = sup pi :: Sup Double
-- >>> -1/0 ?. p
-- True
-- >>> -1/0 ?. filterR (0/0) p
-- False
--
filterR :: Lattice a => a -> Sup a -> Sup a
filterR a p@(Open f l u) = if up' u a then Open (f /\ down' a) (lub l a u) u else p

---------------------------------------------------------------------
-- Internal
---------------------------------------------------------------------


--deriving instance Eq a => Eq (Open k a)
instance Show a => Show (Open k a) where
  show (Open _ l r) = show l ++ " ... " ++ show r

-- |
-- Note that '~~' is strictly weaker than '==', as it ignores the 
-- location of the base point.
instance Finite a => Preorder (Open k a) where
  (Open f _ _) <~ (Open g _ _) = f <~ g

instance Lattice a => Semigroup (Join (Inf a)) where
  (<>) = liftA2 joinInf

instance UpperBounded a => Monoid (Join (Inf a)) where
  mempty = pure $ inf top

instance Lattice a => Semigroup (Meet (Inf a)) where
  (<>) = liftA2 meetInf

instance Bounded a => Monoid (Meet (Inf a)) where
  mempty = pure $ inf bottom


joinInf :: Lattice a => Inf a -> Inf a -> Inf a
joinInf (Open f1 l1 r1) (Open f2 l2 r2) = Open (f1 \/ f2) (l1 /\ l2) (r1 \/ r2)

meetInf :: Lattice a => Inf a -> Inf a -> Inf a
meetInf (Open f1 l1 r1) (Open f2 l2 r2) = Open (f1 /\ f2) (l1 \/ l2) (r1 /\ r2)

instance (Finite a, Lattice a) => Lattice (Inf a)
instance (Finite a, Bounded a) => Bounded (Inf a)


instance Lattice a => Semigroup (Join (Sup a)) where
  (<>) = liftA2 joinSup

instance LowerBounded a => Monoid (Join (Sup a)) where
  mempty = pure $ sup bottom

instance Lattice a => Semigroup (Meet (Sup a)) where
  (<>) = liftA2 meetSup

instance Bounded a => Monoid (Meet (Sup a)) where
  mempty = pure $ sup top

joinSup :: Lattice a => Sup a -> Sup a -> Sup a
joinSup (Open f1 l1 r1) (Open f2 l2 r2) = Open (f1 \/ f2) (l1 \/ l2) (r1 /\ r2)

meetSup :: Lattice a => Sup a -> Sup a -> Sup a
meetSup (Open f1 l1 r1) (Open f2 l2 r2) = Open (f1 /\ f2) (l1 /\ l2) (r1 \/ r2)

instance (Finite a, Lattice a) => Lattice (Sup a)
instance (Finite a, Bounded a) => Bounded (Sup a)

{- 

instance (Finite a, Bounded a) => Heyting (Sup a)
instance (Finite a, Bounded a) => Heyting (Sup a)

instance (Finite a, Bounded a) => Heyting (Inf a) where
  neg (Predicate f) = Predicate $ \a -> neg (f a)
  (Predicate f) <=> (Predicate g) = Predicate $ \a -> f a <=> g a

instance (Finite a, Bounded a) => Quantale (Meet (Inf a)) where
  (\\) = liftA2 impliesOpen
  (//) = flip (\\)

instance (Finite a, Bounded a) => Quantale (Meet (Sup a)) where
  (\\) = liftA2 impliesOpen
  (//) = flip (\\)

negOpen (Open f x) = Open $ \a -> neg (f a)
iffOpen (Open f x) (Open g x) = Open $ \a -> f a <=> g a

impliesOpen :: Bounded a => Open k a -> Open k a -> Open k a
impliesOpen (Open f x) (Open g y) = Open (\a -> f a <= g a) (if x > y then y else top)
-}
