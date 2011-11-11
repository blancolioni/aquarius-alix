with Aquarius.Programs;                 use Aquarius.Programs;

package Ada_Plugin.Ch03 is

   procedure Access_Type_After_Subtype_Mark
     (Access_Type_Tree  : Program_Tree;
      Subtype_Mark      : Program_Tree);

   procedure Array_Type_Definition_Before
     (Array_Type_Tree : Program_Tree);

   procedure Component_Definition_After
     (Item : Program_Tree);

   procedure Derived_Type_After_Qualified_Reference
     (Derived_Type_Tree : Program_Tree;
      Reference_Tree    : Program_Tree);

   procedure Enumerated_Type_Definition_After_Enumerated_Literal
     (Enumerated_Type : Program_Tree;
      Literal         : Program_Tree);

   procedure Enumerated_Type_Definition_Before
     (Enumerated_Type : Program_Tree);

   procedure Modular_Type_Definition_After_Static_Expression
     (Modular_Type : Program_Tree;
      Expression   : Program_Tree);

   procedure Modular_Type_Definition_Before
     (Modular_Type : Program_Tree);

   procedure Interface_Type_After_Interface
     (Interface_Type : Program_Tree;
      Interface_Tree : Program_Tree);

   procedure Private_Type_Definition_Before
     (Private_Type : Program_Tree);

   procedure Range_Type_Definition_After
     (Range_Type_Tree : Program_Tree);

   procedure Record_Type_Definition_Before
     (Record_Type_Tree : Program_Tree);

   procedure Record_Type_Definition_Before_Component
     (Record_Type, Component : Program_Tree);

   procedure Subprogram_Type_Definition_Before
     (Function_Type_Tree : Program_Tree);

   procedure Subtype_Declaration_After
     (Subtype_Declaration : Program_Tree);

   procedure Subtype_Indication_After_Subtype_Mark
     (Indication_Tree, Mark_Tree : Program_Tree);

   procedure Subtype_Mark_After
     (Subtype_Tree : Program_Tree);

   procedure Type_Declaration_After
     (Type_Declaration : Program_Tree);

   procedure Type_Declaration_Before
     (Type_Declaration : Program_Tree);

   procedure Type_Definition_After
     (Type_Definition : Program_Tree);

   procedure Type_Definition_Before
     (Type_Definition : Program_Tree);

   procedure Type_Header_After
     (Type_Header : Program_Tree);

   procedure Type_Indication_After
     (Type_Indication : Program_Tree);

   procedure Variable_Declaration_After
     (Variable_Declaration : Program_Tree);

   procedure List_Of_Declarations_After
     (Decls : Program_Tree);

   procedure Block_Before
     (Block : Program_Tree);

   procedure Transfer_Entry (Source, Dest : Program_Tree);
   procedure Transfer_Type (Source, Dest : Program_Tree);

end Ada_Plugin.Ch03;
