module Course.Core

import Prelude

-- import Prelude(
--     Eq(..)
--   , Ord(..)
--   , Show(..)
--   , Integral(..)
--   , RealFrac(..)
--   , Num(..)
--   , Fractional(..)
--   , Bool(..)
--   , Either(..)
--   , Ordering(..)
--   , Char
--   , Int
--   , Integer
--   , IO
--   , Rational
--   , seq
--   , error
--   , undefined
--   , const
--   , flip
--   , curry
--   , uncurry
--   , id
--   , otherwise
--   , (.)
--   , ($)
--   , (&&)
--   , (||)
--   , not
--   , even
--   , odd
--   , fst
--   , snd
--   )
-- import Data.String(
--   IsString(..)
--   )
-- 
-- import System.IO(
--     getChar
--   )
-- import Data.Function(
--     on
--   )
-- import Control.Arrow(
--     first
--   , second
--   , (&&&)
--   , (***)
--   )
-- import public Data.Char

export
ifThenElse :
  Bool
  -> a
  -> a
  -> a
ifThenElse True t _ =
  t
ifThenElse False _ f =
  f

export
bool :
  a
  -> a
  -> Bool
  -> a
bool f _ False =
  f
bool _ t True =
  t
