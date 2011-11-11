package Ada_Plugin.Ch04.Gen is

   procedure Primary_After_Numeric_Literal
     (Tree    : Program_Tree;
      Num_Lit : Program_Tree);

   procedure Primary_After_Object_Reference
     (Tree             : Program_Tree;
      Object_Reference : Program_Tree);

   procedure Sub_Expression_After (Item : Program_Tree);
   procedure Operator_After (Item : Program_Tree);

end Ada_Plugin.Ch04.Gen;
