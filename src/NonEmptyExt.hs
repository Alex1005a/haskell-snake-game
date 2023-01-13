module NonEmptyExt where

import Relude

{- 
Returns tail of list without last element.
If tail empty return same list
-}
initTail :: NonEmpty a -> NonEmpty a
initTail l@(_ :| []) = l
initTail (x :| xs) = x :| fromMaybe [] (viaNonEmpty init xs)

{-
Replace elements which equal a.
For example, it is necessary for types that compare by identifier.
-}
replace :: (Eq a) => a -> NonEmpty a -> NonEmpty a
replace replaceEl (x :| xs) =
  if x == replaceEl then replaceEl :| xs
  else x :| (replaceEl : filter (/= replaceEl) xs)