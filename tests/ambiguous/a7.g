identifier         ::= standard ada_identifier

top ::= { context_clause } top_level_subprogram_declaration

list_of_context_clauses ::= { context_clause }

context_clause ::= with_clause | use_clause

with_clause ::= ['limited'] ['private'] 'with'

use_clause ::= 'use'

top_level_subprogram_declaration ::= [ 'private' ] subprogram_declaration

subprogram_declaration ::= 'procedure' | 'package'
