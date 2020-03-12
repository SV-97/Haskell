{-# OPTIONS_GHC -Wall #-}

{-
You're given a list of named children and who they're friends with.
Given two of those kids you want to know if they already are friends,
if there exists a "friendship chain" of children connecting them (we then say that they can be friends)
and if it does what that chain looks like or if there's no such chain (we say they can't be friends).
-}

import           Data.Semigroup
import           Debug.Trace

data Kid =
  Kid
    { name    :: String
    , friends :: [Kid]
    }
  deriving (Eq)

instance Show Kid where
  show = name

bob = Kid {name = "Bob", friends = []}

sophie = Kid {name = "Sophie", friends = [dude]}

alice = Kid {name = "Alice", friends = []}

dude = Kid {name = "Dude", friends = [jim, alice, margret]}

margret = Kid {name = "Margret", friends = [sophie]}

jim = Kid {name = "Jim", friends = [bob, alice]}

joe = Kid {name = "Joe", friends = [bob]}

data FriendshipAbility
  = CantBeFriends
  | Via [Kid] -- this should really be Via (Data.List.NonEmpty.NonEmpty Kid)
  | SameKid Kid
  | AlreadyFriends Kid Kid
  deriving (Show, Eq)

instance Semigroup FriendshipAbility where
  CantBeFriends <> _ = CantBeFriends
  _ <> CantBeFriends = CantBeFriends
  AlreadyFriends a b <> AlreadyFriends c d
    | b == c = Via [a, b, d]
    | otherwise = Via [a, b, c, d]
  SameKid a <> b@(AlreadyFriends c d)
    | a == c = b
    | otherwise = Via [a, c, d]
  a@(AlreadyFriends b c) <> SameKid d
    | c == d = a
    | otherwise = Via [b, c, d]
  AlreadyFriends a b <> Via (cs@(c:_))
    | b == c = Via (a : cs)
    | otherwise = Via $ [a, b] ++ cs
  Via cs <> AlreadyFriends a b
    | last cs == a = Via $ cs ++ [b]
    | otherwise = Via $ cs ++ [a, b]
  SameKid a <> b@(Via kids@(c:_))
    | a == c = b
    | otherwise = Via $ a : kids
  a@(Via kids) <> SameKid b
    | last kids == b = a
    | otherwise = Via $ kids ++ [b]
  SameKid a <> SameKid b
    | a == b = SameKid a
    | otherwise = Via [a, b]
  Via as <> Via bs
    | last as == head bs = Via $ as ++ tail bs
    | otherwise = Via $ as ++ bs
  x <> y = trace (show x ++ " <> " ++ show y) CantBeFriends -- debug case - shouldn't actually occur

reverseChain :: FriendshipAbility -> FriendshipAbility
reverseChain (Via xs) = Via $ reverse xs
reverseChain x        = x

knows :: Kid -> Kid -> Bool
a `knows` b = a `elem` friends b || b `elem` friends a

canBeFriends :: Kid -> Kid -> FriendshipAbility
a `canBeFriends` b
  | a == b = SameKid a
(Kid _ []) `canBeFriends` (Kid _ []) = CantBeFriends
a@(Kid _ friends@(_:_)) `canBeFriends` b@(Kid _ _)
  | a `knows` b = AlreadyFriends a b
  | otherwise = SameKid a <> connection <> SameKid b
  where
    connection = head $ dropWhile cantBeFriends (map (`canBeFriends` b) friends)
    cantBeFriends CantBeFriends = True
    cantBeFriends _             = False
a@(Kid _ []) `canBeFriends` b@(Kid _ _)
  | a `knows` b = AlreadyFriends a b
  | otherwise = reverseChain $ canBeFriends b a -- check if there's chain from the other side (the wrong way round)

main :: IO ()
main = print $ canBeFriends margret jim
