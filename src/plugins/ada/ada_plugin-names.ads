with Aquarius.Programs;

package Ada_Plugin.Names is

   procedure Object_Reference_Before
     (Tree : Aquarius.Programs.Program_Tree);

   procedure Defining_Qualified_Reference_Before
     (Tree : Aquarius.Programs.Program_Tree);

   procedure Defining_Qualified_Reference_After
     (Tree       : Aquarius.Programs.Program_Tree);

   procedure Qualified_Reference_After_Identifier
     (Tree       : Aquarius.Programs.Program_Tree;
      Identifier : Aquarius.Programs.Program_Tree);

   procedure Object_Reference_After
     (Item : Aquarius.Programs.Program_Tree);

   procedure Direct_Name_After
     (Item : Aquarius.Programs.Program_Tree);

   procedure Expanded_Name_After
     (Expanded_Name : Aquarius.Programs.Program_Tree);

   procedure Actual_Argument_List_After
     (Argument_List : Aquarius.Programs.Program_Tree);

--     procedure Component_Name_After
--       (Target : access Aquarius.Actions.Actionable'Class);

   procedure Attribute_Reference_After
     (Tree : Aquarius.Programs.Program_Tree);

   procedure Create_Change_Handlers
     (Plugin  : not null access Ada_Plugin_Type'Class);

end Ada_Plugin.Names;
