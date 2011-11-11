with Aquarius.Programs;                 use Aquarius.Programs;

package Ada_Plugin.Ch04 is

   procedure Binary_Operator_After
     (Node : Program_Tree);

   procedure Expression_After
     (Tree : Program_Tree);

   procedure Factor_After_Not
     (Factor    : Program_Tree;
      Not_Token : Program_Tree);

   procedure Primary_After
     (Node : Program_Tree);

   procedure Sub_Expression_After
     (Node : Program_Tree);

   procedure Unary_Operator_After
     (Node : Program_Tree);

end Ada_Plugin.Ch04;
