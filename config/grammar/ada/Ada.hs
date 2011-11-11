module Ada where

import Aquarius

plugin :: Plugin
plugin = primitive

top :: Tree
top = getTree plugin "top"

newPackage :: String -> Tree
newPackage name =
  parse top $ "package " & name & " is end " & name

getFileName :: Tree -> String
getFileName = toFileName . definingName

toFileName :: [String] -> String
toFileName t = (++ ext) . intersperse "-" $ t
   where ext = if isSpec t then ".ads" else ".adb"

isSpec :: Tree -> Bool
isSpec (CompilationUnit t) = isSpec $ get subprogramDeclaration t
isSpec (SubprogramDeclaration t) = isSpec $ chosen t
isSpec (ProcedureDeclaration t) = isEmpty $ get subprogramBody t
isSpec (FunctionDeclaration t) = isEmpty $ get subprogramBody t



generateBody :: Tree -> Tree
