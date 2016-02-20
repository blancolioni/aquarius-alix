with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with Aquarius.Actions;
with Aquarius.Command_Line;
with Aquarius.Config_Paths;
with Aquarius.Configuration;
with Aquarius.Grammars.Aqua_Gen;
with Aquarius.Grammars.Manager;
with Aquarius.Library;
with Aquarius.Loader;
with Aquarius.Messages.Console;
with Aquarius.Names;
with Aquarius.Programs.Arrangements;
with Aquarius.Rendering.Manager;
with Aquarius.Target.Manager;
with Aquarius.Themes;
with Aquarius.Trace;
with Aquarius.Trees.Cursors;

with Aquarius.Version;

with Komnenos.Logging;
with Komnenos.UI;
with Komnenos.UI.Sessions;

with Komnenos.Entities.Source.Aquarius_Source;
with Komnenos.Entities.Aqua_Entities;

procedure Aquarius.Driver is

   procedure Clear_Cache;

   procedure Show_Usage_Text;

   procedure Show_Allocations;

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache is
      use Ada.Directories;
      Path : constant String :=
               Aquarius.Config_Paths.Config_File ("scratch");

      procedure Clear_Cache_Files (Pattern : String);

      -----------------------
      -- Clear_Cache_Files --
      -----------------------

      procedure Clear_Cache_Files (Pattern : String) is
         Search : Search_Type;
         Next   : Directory_Entry_Type;
      begin
         Start_Search (Search, Path, Pattern,
                       Filter => (Ordinary_File => True, others => False));
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Next);
            begin
               Delete_File (Full_Name (Next));
            exception
               when others =>
                  Ada.Text_IO.Put_Line
                    ("clear-cache: failed to delete "
                     & Full_Name (Next));
            end;
         end loop;
         End_Search (Search);
      end Clear_Cache_Files;

   begin
      Clear_Cache_Files ("*.action");
      Clear_Cache_Files ("*.m32");
      Clear_Cache_Files ("*.o32");
   end Clear_Cache;

   ----------------------
   -- Show_Allocations --
   ----------------------

   procedure Show_Allocations is
      Allocated, Freed : Natural;
   begin
      Aquarius.Programs.Get_Allocation_Info (Allocated, Freed);
      Ada.Text_IO.Put_Line
        ("alloc: names ="
         & Natural'Image (Aquarius.Names.Allocated_Name_Count)
         & "; active trees =" & Natural'Image (Allocated)
         & "; free trees =" & Natural'Image (Freed));
   end Show_Allocations;

   ---------------------
   -- Show_Usage_Text --
   ---------------------

   procedure Show_Usage_Text is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Open (File, In_File,
            Aquarius.Configuration.Get_Library_Path & "/usage.txt");
      Set_Output (Standard_Error);

      while not End_Of_File (File) loop
         declare
            Usage_Line : constant String := Get_Line (File);
         begin
            Put_Line (Usage_Line);
         end;
      end loop;

      Close (File);
--        Put ("Debug modes: ");
--        for I in Aquarius.Trace.Debug_Class loop
--           Put (Aquarius.Trace.Debug_Name (I));
--           if I < Aquarius.Trace.Debug_Class'Last then
--              Put (", ");
--              if Col > 64 then
--                 New_Line;
--                 Put ("             ");
--              end if;
--           else
--              New_Line;
--           end if;
--        end loop;

      Set_Output (Standard_Output);

   exception
      when Name_Error =>
         Put_Line (Standard_Error,
                   "Cannot open usage file; check configuration");
         Put_Line (Standard_Error,
                   "Configuration path is '" &
                     Aquarius.Configuration.Get_Library_Path & "'");

      Set_Output (Standard_Output);
   end Show_Usage_Text;

begin

   Aquarius.Library.Initialise
     (Enable_Plugins => Aquarius.Command_Line.Enable_Plugins,
      Show_Paths_In_Messages => True);

   if Command_Line.Version then

      Ada.Text_IO.Put_Line (Version.Version_String);
      return;

   end if;

   if Command_Line.Help then
      Ada.Text_IO.Put_Line (Version.Version_String);
      Show_Usage_Text;
      return;
   end if;

   if Command_Line.Clear_Cache then
      Clear_Cache;
      Ada.Text_IO.Put_Line ("Finished clearing cache");
      return;
   end if;

   Aquarius.Target.Manager.Set_Target (Aquarius.Command_Line.Target);

   if Aquarius.Command_Line.Filter then

      if Aquarius.Command_Line.Input_File = "" then
         Ada.Text_IO.Put_Line ("no input file specified");
         return;
      end if;

      Komnenos.Entities.Aqua_Entities.Create_Aqua_Object (null);

      declare
         use type Aquarius.Grammars.Aquarius_Grammar;
         use type Aquarius.Programs.Program_Tree;
         Grammar     : Aquarius.Grammars.Aquarius_Grammar;
         Input       : Aquarius.Programs.Program_Tree;
         Theme       : Aquarius.Themes.Aquarius_Theme;
      begin
         if Command_Line.Grammar = "" then
            Grammar :=
              Aquarius.Grammars.Manager.Get_Grammar_For_File
              (Command_Line.Input_File);
         else
            Grammar :=
              Aquarius.Grammars.Manager.Get_Grammar (Command_Line.Grammar);
         end if;

         if Grammar = null or else Grammar.Has_Errors then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  "grammar file contains errors; exiting");
            return;
         end if;

         if Command_Line.Enable_Debug /= "" then
            Aquarius.Trace.Enable (Command_Line.Enable_Debug);
            Aquarius.Trace.Start_Trace;
         end if;

         Input :=
           Aquarius.Loader.Load_From_File (Grammar, Command_Line.Input_File);

         if Input = null then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  "Cannot load '" & Command_Line.Input_File &
                                    "'");
            return;
         end if;

         Ada.Text_IO.Put_Line ("done");

         Grammar.Run_Action_Trigger (Input,
                                     Aquarius.Actions.Semantic_Trigger);

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

         if Command_Line.Action /= "" then
            if Command_Line.Action = "grammar-gen" then
               Aquarius.Grammars.Aqua_Gen.Generate
                 (Grammar, ".");
            else
               Grammar.Run_Actions (Command_Line.Action, Input);
            end if;
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
            end if;
         end;

         if Command_Line.Output_File /= "" then

            Aquarius.Programs.Arrangements.Arrange
              (Input,
               Line_Length => Aquarius.Command_Line.Line_Length);

            declare
               Render_Name : constant String :=
                               Command_Line.Renderer;
               Renderer : Aquarius.Rendering.Aquarius_Renderer :=
                               Aquarius.Rendering.Manager.Renderer
                                 (Render_Name);
            begin
               if Command_Line.Theme = "" then
                  Theme := Aquarius.Themes.Active_Theme;
               else
                  Theme := Aquarius.Themes.Load_Theme (Command_Line.Theme);
               end if;

               Renderer.Set_Theme (Theme);

               declare
                  use Ada.Text_IO;
                  Using_File : constant Boolean :=
                                 Command_Line.Output_File /= "";
                  File       : File_Type;
               begin
                  if Using_File then
                     Create (File, Out_File, Command_Line.Output_File);
                     Set_Output (File);
                  end if;

                  Aquarius.Programs.Arrangements.Render
                    (Program   => Input,
                     Renderer  => Renderer,
                     Point     => Aquarius.Trees.Cursors.Left_Of_Tree (Input),
                     Partial   => "");

                  if Using_File then
                     Close (File);
                     Set_Output (Standard_Output);
                  end if;
               end;
            end;
         end if;

         if Command_Line.Enable_Debug /= "" then
            Aquarius.Trace.End_Trace;
         end if;

         Show_Allocations;

      exception
         when others =>
            if Input /= null then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     "Caught exception");
            end if;
            raise;
      end;

   else

      Komnenos.Logging.Start_Logging;

      Show_Allocations;

      declare
         UI   : constant Komnenos.UI.Komnenos_UI :=
                  Komnenos.UI.Create_UI
                    (Aquarius.Config_Paths.Config_Path
                     & "/komnenos");
      begin

         Komnenos.Entities.Aqua_Entities.Create_Aqua_Object (UI);

         if Command_Line.Session_File /= "" then
            Komnenos.UI.Sessions.Load_Session
              (UI, Command_Line.Session_File);
         elsif Ada.Directories.Exists (".aquarius-session") then
            Komnenos.UI.Sessions.Load_Session (UI, ".aquarius-session");
         end if;

         if Command_Line.Input_File /= ""
           or else Command_Line.Extra_Arguments /= ""
         then
            declare
               use Komnenos.Entities.Source.Aquarius_Source;
               Path : constant String :=
                        (if Command_Line.Input_File /= ""
                         then Command_Line.Input_File
                         else Command_Line.Extra_Arguments);
               Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                           Aquarius.Grammars.Manager.Get_Grammar_For_File
                             (Path);
               Input : constant Aquarius.Programs.Program_Tree :=
                         Aquarius.Loader.Load_From_File
                             (Grammar, Path);
               Entity  : constant Komnenos.Entities.Entity_Reference :=
                           Create_Aquarius_Source_Entity
                             (Table            => UI,
                              Name             =>
                                Ada.Directories.Simple_Name
                                  (Path),
                              File_Name        => Path,
                              Class            => "declaration",
                              Line             => 1,
                              Column           => 1,
                              Top_Level        => True,
                              Compilation_Unit => Input,
                              Entity_Spec      => Input,
                              Entity_Body      => null);
               pragma Unreferenced (Entity);
            begin
               Grammar.Run_Action_Trigger
                 (Input,
                  Aquarius.Actions.Semantic_Trigger);

--                 UI.Add_Entity
--                   (Ada.Directories.Simple_Name (Command_Line.Input_File),
--                    Entity);
            end;
         end if;

         Show_Allocations;

         UI.Start;

         Show_Allocations;

         Komnenos.UI.Sessions.Save_Session (UI, ".aquarius-session");

      end;

   end if;

exception
   when E : others =>
      Ada.Text_IO.Put_Line
        ("exception: " & Ada.Exceptions.Exception_Name (E));
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Message (E));

end Aquarius.Driver;
