with Aquarius.Programs;                 use Aquarius.Programs;

package Ada_Plugin.Ch05 is

   procedure Procedure_Call_Before_Object_Reference
     (Call : Program_Tree;
      Ref  : Program_Tree);

   procedure Assignment_After
     (Assignment : Program_Tree);

   procedure Assignment_Before_Expression
     (Assignment : Program_Tree;
      Expression : Program_Tree);

   procedure Boolean_Expression_Before
     (Statement : Program_Tree;
      Condition : Program_Tree);

end Ada_Plugin.Ch05;
