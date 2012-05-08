with Aquarius.Programs;                 use Aquarius.Programs;

package Ada_Plugin.Ch06 is

   procedure Procedure_Specification_Before
     (Spec : Program_Tree);

   procedure Function_Specification_Before
     (Spec : Program_Tree);

   procedure Function_Specification_After_Type_Indication
     (Spec       : Program_Tree;
      Indication : Program_Tree);

   procedure Procedure_Spec_After_Defining_Name
     (Procedure_Spec, Procedure_Name  : Program_Tree);

   procedure Procedure_Declaration_After
     (Procedure_Declaration : Program_Tree);

   procedure Function_Declaration_After
     (Function_Declaration : Program_Tree);

   procedure Spec_Before_Arguments
     (Spec : Program_Tree;
      Args : Program_Tree);

   procedure Spec_After_Arguments
     (Spec : Program_Tree;
      Args : Program_Tree);

   procedure Formal_Argument_After
     (Arg  : Program_Tree);

   procedure Subprogram_Before_Body
     (Spec       : Program_Tree;
      Subprogram : Program_Tree);

end Ada_Plugin.Ch06;
