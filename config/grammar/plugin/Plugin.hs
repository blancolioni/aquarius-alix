module Plugin where

type Property = String

copyProperty :: Tree -> Tree -> Property -> IO Tree
copyProperty to from prop = if hasProp from prop
                                then setProp to $ getProp from prop
                                else return to

declarationAfter :: Tree -> IO Tree
declarationAfter tree = do
  case directChildren tree of
    []     -> error "declaration has no children"
    [d]    -> do when (d $? adaSpec) tree $= adaSpec (d $< adaSpec)
                 when (d $? adaBody) tree $= adaBody (d $< adaBody)
