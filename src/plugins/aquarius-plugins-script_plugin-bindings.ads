with Aquarius.Programs;

package Aquarius.Plugins.Script_Plugin.Bindings is

   procedure Plugin_Declaration_Before_List_Of_Declarations
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

   procedure Property_Declaration_After
     (Item : Aquarius.Programs.Program_Tree);

   procedure Group_Declaration_Before
     (Item : Aquarius.Programs.Program_Tree);

   procedure Group_Declaration_After
     (Item : Aquarius.Programs.Program_Tree);

   procedure Group_Declaration_Before_List_Of_Actions
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

   procedure Group_Declaration_After_List_Of_Actions
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

   procedure Action_After_Action_Time
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

   procedure Action_After_Action_Context
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

   procedure Action_Before_Action_Definition
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

   procedure Action_After_Action_Definition
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

   procedure Expression_After
     (Item : Aquarius.Programs.Program_Tree);

   procedure Let_Expression_After_Binding
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

   procedure Binding_After_Identifier
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

   procedure Binding_After_Expression
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

   procedure Let_Expression_After
     (Item : Aquarius.Programs.Program_Tree);

   procedure Call_Expression_After
     (Item : Aquarius.Programs.Program_Tree);

   procedure With_Expression_After
     (Item : Aquarius.Programs.Program_Tree);

   procedure Literal_Expression_After
     (Item : Aquarius.Programs.Program_Tree);

   procedure Call_Expression_After_Name
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

   procedure Arguments_After_Expression
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

   procedure Call_Expression_After_Arguments
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

end Aquarius.Plugins.Script_Plugin.Bindings;
