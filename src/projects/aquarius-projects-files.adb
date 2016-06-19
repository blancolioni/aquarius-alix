with Ada.Text_IO;

with Aquarius.Actions;
with Aquarius.Grammars.Manager;
with Aquarius.Loader;
with Aquarius.Messages.Console;
with Aquarius.Trees.Properties;

package body Aquarius.Projects.Files is

   ----------------------------
   -- Load_Project_From_File --
   ----------------------------

   function Load_Project_From_File
     (Path : String)
      return Aquarius_Project
   is
      use type Aquarius.Grammars.Aquarius_Grammar;
      use type Aquarius.Programs.Program_Tree;
      Grammar     : constant Aquarius.Grammars.Aquarius_Grammar :=
                      Aquarius.Grammars.Manager.Get_Grammar_For_File
                        (Path);
      Input       : Aquarius.Programs.Program_Tree;
   begin

      if Grammar = null or else Grammar.Has_Errors then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "grammar file contains errors; exiting");
         return null;
      end if;

      Input :=
        Aquarius.Loader.Load_From_File (Grammar, Path);

      if Input = null then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Cannot load '" & Path &
                                 "'");
         return null;
      end if;

      if False then
         Grammar.Run_Action_Trigger (Input,
                                     Aquarius.Actions.Semantic_Trigger);
      end if;

      declare
         use Aquarius.Messages;
         List : Message_List;
      begin
         Input.Get_Messages (List);
         if Message_Count (List) > 0 then
            if Highest_Level (List) > Warning then
               Aquarius.Messages.Console.Show_Messages (List);
            end if;
         else
            Ada.Text_IO.Put_Line ("no messages");
         end if;
      end;

      return Aquarius.Trees.Properties.Get_Project (Input.all);

   end Load_Project_From_File;

end Aquarius.Projects.Files;
