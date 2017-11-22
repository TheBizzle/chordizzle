module CircularZipper(CircularZipper(CircularZipper, focus, lefties, righties), fromList, leftwards, rightwards) where

import Bizzlelude

import Data.List(filter, head, init, last, tail)

data CircularZipper a
  = CircularZipper {
      lefties  :: [a]
    , focus    ::  a
    , righties :: [a]
    }

fromList :: [a] -> CircularZipper a
fromList (h:t) = CircularZipper [] h t
fromList _     = error "Cannot make zipper from empty list"

leftwards :: CircularZipper a -> CircularZipper a
leftwards cz@(CircularZipper    [] x     []) = cz
leftwards    (CircularZipper    [] x rights) = CircularZipper (x:(init rights)) (last rights) []
leftwards    (CircularZipper lefts x rights) = CircularZipper (   init lefts  ) (last  lefts) (x:rights)

rightwards :: CircularZipper a -> CircularZipper a
rightwards cz@(CircularZipper    [] x     []) = cz
rightwards    (CircularZipper lefts x     []) = CircularZipper             [] (head  lefts) ((tail  lefts) <> [x])
rightwards    (CircularZipper lefts x rights) = CircularZipper (lefts <> [x]) (head rights) ( tail rights        )
