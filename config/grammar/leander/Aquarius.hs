module Aquarius where

primParent :: Tree -> Tree
primParent = primitive #primTreeParent

nullTree :: Tree -> Bool
nullTree = primitive #primNullTree

parent :: Tree -> Maybe Tree
parent t = let p = primParent t
           in if nullTree p
              then Nothing
              else Just p
