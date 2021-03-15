{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Most of the connections in this module are only valid up to
-- /maxBound :: Int64/ seconds, i.e. approximately 3 trillion years.
--
module Data.Connection.Time (

    -- * Day
    --intday,
    
    -- * DiffTime
    --f12dft,
    --DiffTime,

    -- * TimeSpec
    tspi64,
    f09tsp,
    rattsp,
    f32tsp,
    f64tsp,
    diffTimeSpec,
    TimeSpec(..),

    -- * Clock
    getTime,
    getRes,
    Clock (..),
) where

import safe Control.Arrow ((&&&))
import safe Control.Monad (guard)
import safe Data.Connection.Conn
import safe Data.Connection.Int
import safe Data.Connection.Fixed
import safe Data.Connection.Float
import safe Data.Connection.Ratio
import safe Data.Fixed
import safe Data.Int

--import safe Data.Time
import safe Data.Order.Extended
import safe Data.Order.Syntax
import safe Prelude hiding (Eq (..), Ord (..), ceiling)
import safe System.Clock

-- Deci

-- | The 'Int64' is valued in seconds
tspi64 :: Conn k TimeSpec Int64
tspi64 = Conn f g h where
  f (normalize -> TimeSpec s n) = s + if n == 0 then 0 else 1
  g i = TimeSpec i 0
  h (normalize -> TimeSpec s _) = s

{-
--tspi64 :: Conn k Integer TimeSpec
tspint :: Conn 'L TimeSpec (Maybe Integer)
tspint = ConnL f g where

  div' = upper2 i64int (liftA2 div)
  mod' = upper2 i64int (liftA2 mod)
  divMod' x = div' x &&& mod' x
  
  f (normalize -> TimeSpec s n) = do
    s' <- ceiling i64int s
    n' <- ceiling i64int n
    return $ s' * s2ns + n'

  g i = let (q, r) = divMod i (Just s2ns) in TimeSpec (upper i64int q) (upper i64int r)

fromPred :: (a -> Bool) -> a -> Maybe a
fromPred p a = a <$ guard (p a)
-}


tspf09 :: Conn 'L TimeSpec Nano
tspf09 = ConnL f g where
  f = MkFixed . toNanoSecs

  g (MkFixed i) = let (q, r) = divMod i s2ns in TimeSpec (fromInteger $ min max64 q) (fromInteger r) 

  max64 = 9223372036854775807

rattsp' :: Conn 'L (Extended TimeSpec) Rational
rattsp' = connL $ ratfix >>> mapped (connR tspf09)


-- | The 'Nano' is valued in seconds to nanosecond precision.
--
-- /NB/ this connection is only valid up to /maxBound :: Int64/ seconds,
-- , i.e. approximately 3 trillion years.
--
f09tsp :: Conn k Nano TimeSpec
f09tsp = Conn f g f where
  --f (MkFixed i) = fromNanoSecs i -- broken on obscenely large numbers
  g = MkFixed . toNanoSecs

  f (MkFixed i) = let (q, r) = divMod i s2ns in TimeSpec (fromInteger $ min max64 q) (fromInteger r) 

  max64 = 9223372036854775807

-- | The 'Rational' is valued in seconds to nanosecond precision.
rattsp :: Conn k Rational (Extended TimeSpec)
rattsp = ratfix >>> mapped f09tsp

-- | The 'Float' is valued in seconds to nanosecond precision.
--
-- >>> Data.Connection.ceiling f32tsp (0/0)
-- Top
-- >>> Data.Connection.ceiling f32tsp pi
-- Extended (TimeSpec {sec = 3, nsec = 141592742})
f32tsp :: Conn 'L Float (Extended TimeSpec)
f32tsp = connL ratf32 >>> rattsp

-- | The 'Double' is valued in seconds to nanosecond precision.
--
-- >>> Data.Connection.ceiling f64tsp (0/0)
-- Top
-- >>> Data.Connection.ceiling f64tsp pi
-- Extended (TimeSpec {sec = 3, nsec = 141592654})
f64tsp :: Conn 'L Double (Extended TimeSpec)
f64tsp = connL ratf64 >>> rattsp

{-
diffSeconds' :: TimeSpec -> TimeSpec -> Double
diffSeconds' end start
  = (* 1e-9)
  $ fromIntegral
  $ toNanoSecs end - toNanoSecs  start

-- | Return the difference between two 'TimeSpec's in seconds
--
-- >>> diffSeconds (TimeSpec 3 141592654) (TimeSpec 10 0)
-- 6.858407346
--
diffSeconds :: TimeSpec -> TimeSpec -> Double
diffSeconds end start = upper f64tsp . Extended $ diffTimeSpec end start
-}

{-
f12dft :: Conn k Pico DiffTime
f12dft = Conn f g f where
  f (MkFixed i) = picosecondsToDiffTime i
  g t = MkFixed (diffTimeToPicoseconds t)

--f12ndt :: Conn k Pico NominalDiffTime

intday :: Conn k Integer Day
intday = Conn f g f where
  f = ModifiedJulianDay
  g = toModifiedJulianDay
-}

-- Internal

-------------------------

s2ns :: Num a => a
s2ns = 10^9

normalize :: TimeSpec -> TimeSpec
normalize (TimeSpec xs xn) | xn < 0 || xn >= s2ns = TimeSpec (xs + q)  r
                           | otherwise            = TimeSpec  xs      xn
                             where (q, r) = xn `divMod` s2ns

triple :: Integer -> Conn k (Fixed e1) (Fixed e2)
triple prec = Conn f g h
  where
    f (MkFixed i) = MkFixed $ let j = i `div` prec in if i `mod` prec == 0 then j else j + 1
    g (MkFixed i) = MkFixed $ i * prec
    h (MkFixed i) = MkFixed $ let j = i `div` prec in if i `mod` prec == 0 then j else j - 1
