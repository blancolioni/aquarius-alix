with Aquarius.Grammars;
with Aquarius.Interaction;
with Aquarius.Programs;
with Aquarius.UI;

package Aquarius.Loader is

   function Load_From_File
     (Grammar    : in     Aquarius.Grammars.Aquarius_Grammar;
      Project    : not null access Programs.Root_Program_Tree_Store'Class;
      Interactor : not null access Interaction.Interactor'Class;
      UI         : Aquarius.UI.Aquarius_UI;
      Path       : in     String)
     return Aquarius.Programs.Program_Tree;

   function Load_From_File
     (Grammar    : in     Aquarius.Grammars.Aquarius_Grammar;
      Path       : in     String)
     return Aquarius.Programs.Program_Tree;
   --  Load file using default project, interactor and UI.
   --     project - new default project
   --     interactor - console
   --     UI - console

   function Load_From_File
     (Path       : in     String)
      return Aquarius.Programs.Program_Tree;
   --  Load file using default project, interactor and UI.
   --  Infer grammar from path extension

   function Load_From_File
     (Grammar    : in     Aquarius.Grammars.Aquarius_Grammar;
      Project    : not null access Programs.Root_Program_Tree_Store'Class;
      UI         : Aquarius.UI.Aquarius_UI;
      Path       : in     String)
     return Aquarius.Programs.Program_Tree;
   --  Load file using default interactor (console)

end Aquarius.Loader;
