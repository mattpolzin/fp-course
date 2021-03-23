module Course.Optional

import Course.Core
import Prelude

-- | The `Optional` data type contains 0 or 1 value.
--
-- It might be thought of as a list, with a maximum length of one.
data Optional a =
  Full a
  | Empty

Eq (Optional a) where
  x == y = ?eqhole

Show (Optional a) where
  show x = ?showhole

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
mapOptional =
  ?mapOptional_rhs

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
bindOptional =
  ?bindOptional_rhs

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
(??) =
  ?coalesce_rhs

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
(<+>) =
  ?combine_rhs

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
optional =
  ?eliminate_rhs

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
