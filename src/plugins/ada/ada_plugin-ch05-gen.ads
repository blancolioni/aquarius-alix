with Aquarius.Programs;

package Ada_Plugin.Ch05.Gen is

   procedure Assignment_After
     (Assignment : Aquarius.Programs.Program_Tree);

   procedure Statement_After
     (Statement  : Aquarius.Programs.Program_Tree);

   procedure Transfer_Tagatha_Fragment
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

   procedure Save_Tagatha_Fragment
     (Tree : Aquarius.Programs.Program_Tree);

   procedure While_Statement_Header_After
     (Tree : Aquarius.Programs.Program_Tree);

   procedure Loop_Statement_Before
     (Tree : Aquarius.Programs.Program_Tree);

   procedure Loop_Statement_After
     (Tree : Aquarius.Programs.Program_Tree);

end Ada_Plugin.Ch05.Gen;
