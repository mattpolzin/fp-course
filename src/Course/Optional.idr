module Course.Optional

import Course.Core
import Prelude

%default total

-- | The `Optional` data type contains 0 or 1 value.
--
-- It might be thought of as a list, with a maximum length of one.
data Optional a = Full a
                | Empty

-- Eq & Show allow easy comparison and printing of Optional
-- values.
Eq a => Eq (Optional a) where
  (Full x) == (Full y) = x == y
  Empty    == Empty    = True
  _        == _        = False

Show a => Show (Optional a) where
  show Empty    = "Empty"
  show (Full x) = "Full " ++ (show x)

-- | Map the given function on the possible value.
--
-- >>> mapOptional (+1) Empty
-- Empty
--
-- >>> mapOptional (+1) (Full 8)
-- Full 9
mapOptional :
  (a -> b)
  -> Optional a
  -> Optional b
mapOptional f (Full x) = Full (f x)
mapOptional f Empty = Empty
--   ?mapOptional_rhs

-- | Bind the given function on the possible value.
--
-- >>> bindOptional Full Empty
-- Empty
--
-- >>> bindOptional (\n => if even n then Full (n - 1) else Full (n + 1)) (Full 8)
-- Full 7
--
-- >>> bindOptional (\n => if even n then Full (n - 1) else Full (n + 1)) (Full 9)
-- Full 10
bindOptional :
  (a -> Optional b)
  -> Optional a
  -> Optional b
bindOptional f Empty = Empty
bindOptional f (Full x) = case f x of
                               (Full y) => Full y
                               Empty => Empty
--   ?bindOptional_rhs

-- | Return the possible value if it exists; otherwise, the second argument.
--
-- >>> Full 8 ?? 99
-- 8
--
-- >>> Empty ?? 99
-- 99
(??) :
  Optional a
  -> a
  -> a
(??) (Full x) _ = x
(??) Empty y = y
--   ?coalesce_rhs

infixl 2 ??

-- | Try the first optional for a value. If it has a value, use it; otherwise,
-- use the second value.
--
-- >>> Full 8 <+> Empty
-- Full 8
--
-- >>> Full 8 <+> Full 9
-- Full 8
--
-- >>> Empty <+> Full 9
-- Full 9
--
-- >>> Empty <+> Empty
-- Empty
(<+>) :
  Optional a
  -> Optional a
  -> Optional a
(<+>) (Full x) _ = Full x
(<+>) Empty y = y

-- | Replaces the Full and Empty constructors in an optional.
--
-- >>> optional (+1) 0 (Full 8)
-- 9
--
-- >>> optional (+1) 0 Empty
-- 0
optional :
  (a -> b)
  -> b
  -> Optional a
  -> b
optional f empty (Full x) = f x
optional f empty Empty = empty

applyOptional : Optional (a -> b) -> Optional a -> Optional b
applyOptional f a = bindOptional (\f' => mapOptional f' a) f

twiceOptional : (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f = applyOptional . mapOptional f

contains : Eq a => a -> Optional a -> Bool
contains _ Empty = False
contains a (Full z) = a == z

implementation Functor Optional where
  map f (Full x) = Full (f x)
  map f Empty = Empty

implementation Applicative Optional where
  (Full g) <*> (Full x) = Full (g x)
  Empty <*> _ = Empty
  _ <*> Empty = Empty

  pure =
    Full

implementation Monad Optional where
  (>>=) =
    flip bindOptional
