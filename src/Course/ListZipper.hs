{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ListZipper where

import Course.Applicative
import Course.Comonad
import Course.Core
import Course.Extend
import Course.Functor
import Course.List
import Course.Optional
import Course.Traversable
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.Maybe(maybe)
-- >>> import Course.Core
-- >>> import qualified Prelude as P
-- >>> let optional e _ Empty = e; optional _ f (Full a) = f a
-- >>> instance Arbitrary a => Arbitrary (Optional a) where arbitrary = P.fmap (maybe Empty Full) arbitrary
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap (P.foldr (:.) Nil :: ([a] -> List a)) arbitrary
-- >>> instance Arbitrary a => Arbitrary (ListZipper a) where arbitrary = do l <- arbitrary; x <- arbitrary; r <- arbitrary; P.return (ListZipper l x r)

-- A `ListZipper` is a focussed position, with a list of values to the left and to the right.
--
-- For example, taking the list [0,1,2,3,4,5,6], the moving focus to the third position, the zipper looks like:
-- ListZipper [2,1,0] 3 [4,5,6]
--
-- Supposing then we move left on this zipper:
-- ListZipper [1,0] 2 [3,4,5,6]
--
-- then suppose we add 17 to the focus of this zipper:
-- ListZipper [1,0] 19 [3,4,5,6]
data ListZipper a
  = ListZipper (List a) a (List a)
  deriving (Eq)

lefts ::
  ListZipper a ->
  List a
lefts (ListZipper l _ _) =
  l

rights ::
  ListZipper a ->
  List a
rights (ListZipper _ _ r) =
  r

-- A `MaybeListZipper` is a data structure that allows us to "fail" zipper operations.
-- e.g. Moving left when there are no values to the left.
newtype MaybeListZipper a
  = MLZ (Optional (ListZipper a))
  deriving (Eq)

isZ ::
  ListZipper a ->
  MaybeListZipper a
isZ = MLZ . Full

isNotZ ::
  MaybeListZipper a
isNotZ = MLZ Empty

-- | Implement the `Functor` instance for `ListZipper`.
--
-- >>> (+1) <$> (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2] >5< [6,7,8]
instance Functor ListZipper where
  (<$>) f (ListZipper l a r) = ListZipper (f <$> l) (f a) (f <$> r)

-- | Implement the `Functor` instance for `MaybeListZipper`.
--
-- >>> (+1) <$> (MLZ (Full (zipper [3,2,1] 4 [5,6,7])))
-- [4,3,2] >5< [6,7,8]
instance Functor MaybeListZipper where
  (<$>) f (MLZ a) = MLZ $ (f <$>) <$> a

-- | Convert the given zipper back to a list.
--
-- >>> toList <$> toOptional (fromList Nil)
-- Empty
--
-- >>> toList (ListZipper Nil 1 (2:.3:.4:.Nil))
-- [1,2,3,4]
--
-- >>> toList (ListZipper (3:.2:.1:.Nil) 4 (5:.6:.7:.Nil))
-- [1,2,3,4,5,6,7]
toList ::
  ListZipper a ->
  List a
toList (ListZipper l a r) = toList' l a r
  where
    toList' Nil a' r' = a' :. r'
    toList' (h :. t) a' r' = toList' t h (a' :. r')

-- | Convert the given (maybe) zipper back to a list.
toListZ ::
  MaybeListZipper a ->
  List a
toListZ (MLZ Empty) =
  Nil
toListZ (MLZ (Full z)) =
  toList z

-- | Create a `MaybeListZipper` positioning the focus at the head.
--
-- ->>> fromList (1 :. 2 :. 3 :. Nil)
-- [] >1< [2,3]
--
-- >>> fromList Nil
-- ><
--
-- prop> \xs -> xs == toListZ (fromList xs)
fromList ::
  List a ->
  MaybeListZipper a
fromList Nil = MLZ Empty
fromList (h :. t) = isZ $ ListZipper Nil h t

-- | Retrieve the `ListZipper` from the `MaybeListZipper` if there is one.
--
-- prop> \xs -> isEmpty xs == (toOptional (fromList xs) == Empty)
--
-- prop> \z -> toOptional (fromOptional z) == z
toOptional ::
  MaybeListZipper a ->
  Optional (ListZipper a)
toOptional (MLZ a) = a

zipper ::
  [a] ->
  a ->
  [a] ->
  ListZipper a
zipper l x r =
  ListZipper (listh l) x (listh r)

fromOptional ::
  Optional (ListZipper a) ->
  MaybeListZipper a
fromOptional =
  MLZ

asZipper ::
  (ListZipper a -> ListZipper a) ->
  MaybeListZipper a ->
  MaybeListZipper a
asZipper f =
  asMaybeZipper (isZ . f)

(>$>) ::
  (ListZipper a -> ListZipper a) ->
  MaybeListZipper a ->
  MaybeListZipper a
(>$>) =
  asZipper

asMaybeZipper ::
  (ListZipper a -> MaybeListZipper a) ->
  MaybeListZipper a ->
  MaybeListZipper a
asMaybeZipper _ (MLZ Empty) =
  isNotZ
asMaybeZipper f (MLZ (Full z)) =
  f z

(-<<) ::
  (ListZipper a -> MaybeListZipper a) ->
  MaybeListZipper a ->
  MaybeListZipper a
(-<<) =
  asMaybeZipper

-- | Update the focus of the zipper with the given function on the current focus.
--
-- >>> withFocus (+1) (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> withFocus (+1) (zipper [1,0] 2 [3,4])
-- [1,0] >3< [3,4]
withFocus ::
  (a -> a) ->
  ListZipper a ->
  ListZipper a
withFocus f (ListZipper l a r) = ListZipper l (f a) r

-- | Set the focus of the zipper to the given value.
-- /Tip:/ Use `withFocus`.
--
-- >>> setFocus 1 (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> setFocus 1 (zipper [1,0] 2 [3,4])
-- [1,0] >1< [3,4]
setFocus ::
  a ->
  ListZipper a ->
  ListZipper a
setFocus a = withFocus (const a)

-- A flipped infix alias for `setFocus`. This allows:
--
-- z .= "abc" -- sets the focus on the zipper z to the value "abc".
(.=) ::
  ListZipper a ->
  a ->
  ListZipper a
(.=) =
  flip setFocus

-- | Returns whether there are values to the left of focus.
--
-- >>> hasLeft (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasLeft (zipper [] 0 [1,2])
-- False
hasLeft ::
  ListZipper a ->
  Bool
hasLeft (ListZipper l _ _) = not $ isEmpty l

-- | Returns whether there are values to the right of focus.
--
-- >>> hasRight (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasRight (zipper [1,0] 2 [])
-- False
hasRight ::
  ListZipper a ->
  Bool
hasRight (ListZipper _ _ r) = not $ isEmpty r

-- | Seek to the left for a location matching a predicate, excluding the
-- focus.
--
-- /Tip:/ Use `break`
--
-- prop> \xs p -> findLeft (const p) -<< fromList xs == isNotZ
--
-- >>> findLeft (== 1) (zipper [2, 1] 3 [4, 5])
-- [] >1< [2,3,4,5]
--
-- >>> findLeft (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findLeft (== 1) (zipper [2, 1] 1 [4, 5])
-- [] >1< [2,1,4,5]
--
-- >>> findLeft (== 1) (zipper [1, 2, 1] 3 [4, 5])
-- [2,1] >1< [3,4,5]
--
-- >>> findLeft (== 1) (zipper [3, 4, 1, 5] 9 [2, 7])
-- [5] >1< [4,3,9,2,7]
findLeft ::
  (a -> Bool) ->
  ListZipper a ->
  MaybeListZipper a
findLeft p (ListZipper l a r) =
  case break p l of
    (_, Nil) -> isNotZ
    (r', a' :. l') -> isZ $ ListZipper l' a' (reverse r' ++ a :. r)

-- | Seek to the right for a location matching a predicate, excluding the
-- focus.
--
-- /Tip:/ Use `break`
--
-- prop> \xs -> findRight (const False) -<< fromList xs == isNotZ
--
-- >>> findRight (== 5) (zipper [2, 1] 3 [4, 5])
-- [4,3,2,1] >5< []
--
-- >>> findRight (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [4, 5, 1])
-- [5,4,1,2,3] >1< []
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [1, 4, 5, 1])
-- [1,2,3] >1< [4,5,1]
findRight ::
  (a -> Bool) ->
  ListZipper a ->
  MaybeListZipper a
findRight p (ListZipper l a r) =
  case break p r of
    (_, Nil) -> isNotZ
    (l', a' :. r') -> isZ $ ListZipper (reverse l' ++ a :. l) a' r'

-- | Move the zipper left, or if there are no elements to the left, go to the far right.
--
-- >>> moveLeftLoop (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftLoop (zipper [] 1 [2,3,4])
-- [3,2,1] >4< []
moveLeftLoop ::
  ListZipper a ->
  ListZipper a
moveLeftLoop z@(ListZipper Nil _ Nil) = z
moveLeftLoop (ListZipper (a' :. l') a r) = ListZipper l' a' (a :. r)
moveLeftLoop (ListZipper Nil a r) =
  let (a' :. l') = reverse (a :. r)
   in ListZipper l' a' Nil

-- | Move the zipper right, or if there are no elements to the right, go to the far left.
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [])
-- [] >1< [2,3,4]
moveRightLoop ::
  ListZipper a ->
  ListZipper a
moveRightLoop z@(ListZipper Nil _ Nil) = z
moveRightLoop (ListZipper l a (a' :. r')) = ListZipper (a :. l) a' r'
moveRightLoop (ListZipper l a Nil) =
  let (a' :. r') = reverse (a :. l)
   in ListZipper Nil a' r'

-- | Move the zipper one position to the left.
--
-- >>> moveLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeft (zipper [] 1 [2,3,4])
-- ><
moveLeft ::
  ListZipper a ->
  MaybeListZipper a
moveLeft (ListZipper (a' :. l') a r) = isZ $ ListZipper l' a' (a :. r)
moveLeft (ListZipper Nil _ _) = isNotZ

-- | Move the zipper one position to the right.
--
-- >>> moveRight (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRight (zipper [3,2,1] 4 [])
-- ><
moveRight ::
  ListZipper a ->
  MaybeListZipper a
moveRight (ListZipper l a (a' :. r')) = isZ $ ListZipper (a :. l) a' r'
moveRight (ListZipper _ _ Nil) = isNotZ

-- | Swap the current focus with the value to the left of focus.
--
-- >>> swapLeft (zipper [3,2,1] 4 [5,6,7])
-- [4,2,1] >3< [5,6,7]
--
-- >>> swapLeft (zipper [] 1 [2,3,4])
-- ><
swapLeft ::
  ListZipper a ->
  MaybeListZipper a
swapLeft (ListZipper Nil _ _) = isNotZ
swapLeft (ListZipper (a' :. l') a r) = isZ $ ListZipper (a :. l') a' r

-- | Swap the current focus with the value to the right of focus.
--
-- >>> swapRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [4,6,7]
--
-- >>> swapRight (zipper [3,2,1] 4 [])
-- ><
swapRight ::
  ListZipper a ->
  MaybeListZipper a
swapRight (ListZipper _ _ Nil) = isNotZ
swapRight (ListZipper l a (a' :. r')) = isZ $ ListZipper l a' (a :. r')

-- | Drop all values to the left of the focus.
--
-- >>> dropLefts (zipper [3,2,1] 4 [5,6,7])
-- [] >4< [5,6,7]
--
-- >>> dropLefts (zipper [] 1 [2,3,4])
-- [] >1< [2,3,4]
--
-- prop> \l x r -> dropLefts (zipper l x r) == zipper [] x r
dropLefts ::
  ListZipper a ->
  ListZipper a
dropLefts (ListZipper _ a r) = ListZipper Nil a r

-- | Drop all values to the right of the focus.
--
-- >>> dropRights (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >4< []
--
-- >>> dropRights (zipper [3,2,1] 4 [])
-- [3,2,1] >4< []
--
-- prop> \l x r -> dropRights (zipper l x r) == zipper l x []
dropRights ::
  ListZipper a ->
  ListZipper a
dropRights (ListZipper l a _) = ListZipper l a Nil

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
--
-- >>> moveLeftN 2 (zipper [2,1,0] 3 [4,5,6])
-- [0] >1< [2,3,4,5,6]
--
-- >>> moveLeftN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [3,2,1,0] >4< [5,6]
moveLeftN ::
  Int ->
  ListZipper a ->
  MaybeListZipper a
moveLeftN 0 z = isZ z
moveLeftN n z | n > 0 = moveLeftN (n - 1) -<< moveLeft z
moveLeftN n z = moveLeftN (n + 1) -<< moveRight z

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
--
-- >>> moveRightN 1 (zipper [2,1,0] 3 [4,5,6])
-- [3,2,1,0] >4< [5,6]
--
-- >>> moveRightN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [1,0] >2< [3,4,5,6]
moveRightN ::
  Int ->
  ListZipper a ->
  MaybeListZipper a
moveRightN 0 z = isZ z
moveRightN n z | n > 0 = moveRightN (n - 1) -<< moveRight z
moveRightN n z = moveRightN (n + 1) -<< moveLeft z

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveLeftN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [5,4,3,2,1] >6< [7]
--
-- >>> moveLeftN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7,8,9])
-- Left 3
--
-- >>> moveLeftN' (-4) (zipper [5,4,3,2,1] 6 [7,8,9])
-- Left 3
--
-- >>> rights <$> moveLeftN' 1 (zipper [3,2,error "moveLeftN' not sufficiently lazy"] 4 [5,6,7])
-- Right [4,5,6,7]
moveLeftN' ::
  Int ->
  ListZipper a ->
  Either Int (ListZipper a)
moveLeftN' n z@(ListZipper l _ r) =
  case moveLeftN n z of
    MLZ Empty ->
      if n > 0
        then Left $ length l
        else Left $ length r
    MLZ (Full z') ->
      Right z'

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveRightN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveRightN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [4,3,2,1] >5< [6,7]
--
-- >>> moveRightN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveRightN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [1] >2< [3,4,5,6,7]
--
-- >>> moveRightN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
moveRightN' ::
  Int ->
  ListZipper a ->
  Either Int (ListZipper a)
moveRightN' n z@(ListZipper l _ r) =
  case moveRightN n z of
    MLZ Empty ->
      if n > 0
        then Left $ length r
        else Left $ length l
    MLZ (Full z') ->
      Right z'

-- | Move the focus to the given absolute position in the zipper. Traverse the zipper only to the extent required.
--
-- >>> nth 1 (zipper [3,2,1] 4 [5,6,7])
-- [1] >2< [3,4,5,6,7]
--
-- >>> nth 5 (zipper [3,2,1] 4 [5,6,7])
-- [5,4,3,2,1] >6< [7]
--
-- >>> nth 8 (zipper [3,2,1] 4 [5,6,7])
-- ><
nth ::
  Int ->
  ListZipper a ->
  MaybeListZipper a
nth n z@(ListZipper l _ _) =
  let nl = length l
   in case compare n nl of
        LT -> moveLeftN (nl - n) z
        EQ -> isZ z
        GT -> moveRightN (n - nl) z

-- | Return the absolute position of the current focus in the zipper.
--
-- >>> index (zipper [3,2,1] 4 [5,6,7])
-- 3
--
-- prop> \i z -> optional True (\z' -> index z' == i) (toOptional (nth i z))
index ::
  ListZipper a ->
  Int
index (ListZipper l _ _) = length l

-- | Move the focus to the end of the zipper.
--
-- >>> end (zipper [3,2,1] 4 [5,6,7])
-- [6,5,4,3,2,1] >7< []
--
-- prop> \lz -> toList lz == toList (end lz)
--
-- prop> \lz -> rights (end lz) == Nil
end ::
  ListZipper a ->
  ListZipper a
end z@(ListZipper _ _ r) =
  case moveRightN (length r) z of
    MLZ (Full z') -> z'
    MLZ Empty -> z

-- | Move the focus to the start of the zipper.
--
-- >>> start (zipper [3,2,1] 4 [5,6,7])
-- [] >1< [2,3,4,5,6,7]
--
-- prop> \lz -> toList lz == toList (start lz)
--
-- prop> \lz -> lefts (start lz) == Nil
start ::
  ListZipper a ->
  ListZipper a
start z@(ListZipper l _ _) =
  case moveLeftN (length l) z of
    MLZ (Full z') -> z'
    MLZ Empty -> z

-- | Delete the current focus and pull the left values to take the empty position.
--
-- >>> deletePullLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [5,6,7]
--
-- >>> deletePullLeft (zipper [] 1 [2,3,4])
-- ><
deletePullLeft ::
  ListZipper a ->
  MaybeListZipper a
deletePullLeft (ListZipper Nil _ _) = isNotZ
deletePullLeft (ListZipper (a :. l) _ r) = isZ $ ListZipper l a r

-- | Delete the current focus and pull the right values to take the empty position.
--
-- >>> deletePullRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [6,7]
--
-- >>> deletePullRight (zipper [3,2,1] 4 [])
-- ><
deletePullRight ::
  ListZipper a ->
  MaybeListZipper a
deletePullRight (ListZipper _ _ Nil) = isNotZ
deletePullRight (ListZipper l _ (a :. r)) = isZ $ ListZipper l a r

-- | Insert at the current focus and push the left values to make way for the new position.
--
-- >>> insertPushLeft 15 (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >15< [5,6,7]
--
-- >>> insertPushLeft 15 (zipper [] 1 [2,3,4])
-- [1] >15< [2,3,4]
--
-- prop> \i z -> optional False (==z) (toOptional (deletePullLeft (insertPushLeft i z)))
insertPushLeft ::
  a ->
  ListZipper a ->
  ListZipper a
insertPushLeft a' (ListZipper l a r) = ListZipper (a :. l) a' r

-- | Insert at the current focus and push the right values to make way for the new position.
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >15< [4,5,6,7]
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [])
-- [3,2,1] >15< [4]
--
-- prop> \i z -> optional False (==z) (toOptional (deletePullRight (insertPushRight i z)))
insertPushRight ::
  a ->
  ListZipper a ->
  ListZipper a
insertPushRight a' (ListZipper l a r) = ListZipper l a' (a :. r)

-- | Implement the `Applicative` instance for `ListZipper`.
-- `pure` produces an infinite list zipper (to both left and right).
-- (<*>) zips functions with values by function application.
--
-- prop> \n -> all . (==) <*> take n . lefts . pure
--
-- prop> \n -> all . (==) <*> take n . rights . pure
--
-- >>> zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)] <*> zipper [3,2,1] 4 [5,6,7]
-- [5,12] >8< [15,24,12]
instance Applicative ListZipper where
  -- /Tip:/ Use @List#repeat@.
  pure a = ListZipper (repeat a) a (repeat a)

  -- /Tip:/ Use `zipWith`
  (<*>) (ListZipper fl fa fr) (ListZipper l a r) =
    ListZipper
      (zipWith (\f x -> f x) fl l)
      (fa a)
      (zipWith (\f x -> f x) fr r)

-- | Implement the `Applicative` instance for `MaybeListZipper`.
--
-- /Tip:/ Use @pure@ for `ListZipper`.
-- /Tip:/ Use `<*>` for `ListZipper`.
--
-- prop> \z n -> let is (MLZ (Full z)) = z in all . (==) <*> take n . lefts . is . pure
--
-- prop> \z n -> let is (MLZ (Full z)) = z in all . (==) <*> take n . rights . is . pure
--
-- >>> isZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> isZ (zipper [3,2,1] 4 [5,6,7])
-- [5,12] >8< [15,24,12]
--
-- >>> isNotZ <*> isZ (zipper [3,2,1] 4 [5,6,7])
-- ><
--
-- >>> isZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> isNotZ
-- ><
--
-- >>> isNotZ <*> isNotZ
-- ><
instance Applicative MaybeListZipper where
  pure = isZ . pure
  (<*>) (MLZ f) (MLZ a) = MLZ $ lift2 (<*>) f a

-- | Implement the `Extend` instance for `ListZipper`.
-- This implementation "visits" every possible zipper value derivable from a given zipper (i.e. all zippers to the left and right).
--
-- /Tip:/ Use @List#unfoldr@.
--
-- >>> id <<= (zipper [2,1] 3 [4,5])
-- [[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
instance Extend ListZipper where
  (<<=) f z = ListZipper (unfoldr lExtend z) (f z) (unfoldr rExtend z)
    where
      lExtend z' = apply <$> toOptional (moveLeft z')
      rExtend z' = apply <$> toOptional (moveRight z')
      apply z'' = (f z'', z'')

-- | Implement the `Extend` instance for `MaybeListZipper`.
-- This instance will use the `Extend` instance for `ListZipper`.
--
--
-- id <<= isNotZ
-- ><
--
-- >>> id <<= (isZ (zipper [2,1] 3 [4,5]))
-- [[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
instance Extend MaybeListZipper where
  (<<=) f (MLZ oz) = MLZ $ ((f . isZ) <<=) <$> oz

-- | Implement the `Comonad` instance for `ListZipper`.
-- This implementation returns the current focus of the zipper.
--
-- >>> copure (zipper [2,1] 3 [4,5])
-- 3
instance Comonad ListZipper where
  copure (ListZipper _ a _) = a

-- | Implement the `Traversable` instance for `ListZipper`.
-- This implementation traverses a zipper from left to right while running
-- some `Applicative` effect through the zipper.
-- An effectful zipper is returned.
--
-- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7])
-- Full [1,2,3] >4< [5,6,7]
--
-- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Empty, Full 6, Full 7])
-- Empty
--
-- >>> traverse id (zipper [error "traversing left values in wrong order", Empty] (error "traversing focus before left values") [Full 5, Full 6, Full 7])
-- Empty
instance Traversable ListZipper where
  traverse f (ListZipper l a r) =
    let lk = traverse f l
        lak = lift2 (\y x -> (x, y, Nil)) (f a) lk
        lark = foldLeft (\k x -> lift2 add (f x) k) lak r
        add x (l', a', r') = (l', a', x :. r')
        makeZ (l', a', r') = ListZipper l' a' (reverse r')
     in makeZ <$> lark

-- | Implement the `Traversable` instance for `MaybeListZipper`.
--
-- /Tip:/ Use `traverse` for `ListZipper`.
--
-- >>> traverse id isNotZ
-- ><
--
-- >>> traverse id (isZ (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7]))
-- Full [1,2,3] >4< [5,6,7]
instance Traversable MaybeListZipper where
  traverse f (MLZ oz) = MLZ <$> traverse (traverse f) oz

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Show a => Show (ListZipper a) where
  show (ListZipper l x r) =
    stringconcat [show l, " >", show x, "< ", show r]

instance Show a => Show (MaybeListZipper a) where
  show (MLZ (Full z)) = show z
  show (MLZ Empty) = "><"
