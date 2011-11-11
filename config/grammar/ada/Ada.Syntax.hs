module Ada.Syntax where

data TopLevel = TopLevel CompilationUnit

data CompilationUnit = CompilationUnit [ ContextClause ] TopLevelSubprogamDeclaration

data ListOfContextClauses = ListOfContextClauses [ContextClause]

data TopLevelSubprogramDeclaration =
  TopLevelSubprogramDeclaration Bool SubprogramDeclaration

data ContextClause = WithClause | UseClause

data WithClause = WithClause Bool Bool [ WithedUnitName ]
data UseClause = UseClause [ExternalUnitName]


data NonTerminal =
    TopLevel
  | CompilationUnit
  | ContextClause
  | TopLevelSubprogramDeclaration
  | ListOfContextClauses
  |


