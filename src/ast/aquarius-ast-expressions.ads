package Aquarius.AST.Expressions is

   type Expression_Node is abstract new Root_Tree_Node with private;

   type Expression is access all Expression_Node'Class;

private

   type Expression_Node is abstract new Root_Tree_Node with null record;

end Aquarius.AST.Expressions;
