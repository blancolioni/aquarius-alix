with Aquarius.Programs;                 use Aquarius.Programs;

package Ada_Plugin.Ch10 is

   procedure Compilation_Unit_Before
     (Unit : Program_Tree);

   procedure Procedure_Spec_After_Defining_Name
     (Procedure_Spec, Procedure_Name  : Program_Tree);

   procedure With_Clause_After
     (With_Clause : Program_Tree);

end Ada_Plugin.Ch10;
