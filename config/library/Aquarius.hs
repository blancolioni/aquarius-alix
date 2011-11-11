module Aquarius where

data Node = Terminal String
          | Choice
          | Optional
          | Sequence
          deriving (Show)

data Tree = Tree (Maybe String) Node [Tree]
            deriving (Show)

treeLabel :: Tree -> Maybe String
treeLabel (Tree label _ _) = label

treeNode :: Tree -> Node
treeNode (Tree _ node _) = node

treeChildren :: Tree -> [Tree]
treeChildren (Tree _ _ children) = children

bfs :: (Tree -> Bool) -> (Tree -> Bool) -> [Tree] -> Maybe Tree
bfs _ _ [] = Nothing
bfs good bad ts = case thisLevel ts of
                    (Just t, _) -> Just t
                    (_, True)   -> Nothing
                    _           -> bfs good bad nextLevel
  where nextLevel = concatMap treeChildren ts
        check t | good t    = (Just t, False)
                | bad t     = (Nothing, True)
                | otherwise = (Nothing, False)
        thisLevel [] = (Nothing,False)
        thisLevel (x:xs) = case check x of
                             (Nothing,False) -> thisLevel xs
                             match           -> match

get :: String -> Tree -> Maybe Tree
get name (Tree _ _ ts) = bfs matchName stopAtName ts
  where matchName (Tree (Just nm) _ _) = nm == name
        matchName _ = False
        stopAtName (Tree (Just _) _ _) = True
        stopAtName _ = False

compilationUnit :: Tree
compilationUnit = Tree (Just "compilation_unit") Sequence [subprDecl]

subprDecl :: Tree
subprDecl = Tree (Just "subprogram_declaration") Choice [packageDecl]

packageDecl :: Tree
packageDecl = Tree (Just "package_declaration") Sequence [Tree Nothing Choice [packageSpec]]

packageSpec :: Tree
packageSpec = Tree (Just "package_spec") Sequence [definingPackageName]

definingPackageName :: Tree
definingPackageName = Tree (Just "qualified_reference") Sequence
                           [Tree (Just "identifier") (Terminal "Aquarius") []]

