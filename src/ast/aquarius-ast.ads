with Aquarius.Code;

with Aquarius.Source;

package Aquarius.AST is

   type Root_Tree_Node is abstract tagged private;

   procedure Generate (Node   : access Root_Tree_Node;
                       Gen    : access Aquarius.Code.Generator);

   type AST is access all Root_Tree_Node'Class;

private

   type Root_Tree_Node is abstract tagged null record;

   procedure Create_Root_Node
     (Node     :    out Root_Tree_Node;
      Position : in     Aquarius.Source.Source_Position);

end Aquarius.AST;
