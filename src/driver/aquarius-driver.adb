with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with Tropos.Reader;

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
with Aquarius.Plugins.Manager;
with Aquarius.Programs.Arrangements;
with Aquarius.Projects.Files;
with Aquarius.Rendering.Manager;
with Aquarius.Target.Manager;
with Aquarius.Trace;

with Aquarius.Version;

with Aquarius.Grammars.UI;

with Ack.Errors;
with Ack.IO;

with Ack.Parser;
with Ack.Semantic;
with Ack.Generate;

with Komnenos.Logging;
with Komnenos.Paths;
with Komnenos.Themes;
with Komnenos.UI;
with Komnenos.UI.Sessions;

--  with Komnenos.Entities.Source.Aquarius_Source;
--  with Komnenos.Entities.Aqua_Entities;

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
      Show_Paths_In_Messages => False);

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

   Komnenos.Themes.Load_Theme
     (Tropos.Reader.Read_Config
        (Komnenos.Paths.Config_File ("themes/default.theme")));

   Aquarius.Target.Manager.Set_Target (Aquarius.Command_Line.Target);

   if Aquarius.Command_Line.Filter then

      if Aquarius.Command_Line.Input_File = "" then
         Ada.Text_IO.Put_Line ("no input file specified");
         return;
      end if;

--      Komnenos.Entities.Aqua_Entities.Create_Aqua_Object (null);

      declare
         use type Aquarius.Grammars.Aquarius_Grammar;
         use type Aquarius.Programs.Program_Tree;
         Grammar     : Aquarius.Grammars.Aquarius_Grammar;
         Input       : Aquarius.Programs.Program_Tree;
         Theme       : Komnenos.Themes.Komnenos_Theme;
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

               Aquarius.Grammars.Aqua_Gen.Generate (Grammar);

            elsif Command_Line.Action = "import-aqua" then

               declare
                  Node : constant Ack.Node_Id :=
                           Ack.Parser.Import
                             (Input);
               begin

                  if False then
                     Ack.IO.Put_Line (Node);
                  end if;

                  Ada.Text_IO.Put_Line
                    ("analysing: " & Input.Source_File_Name);

                  Ack.Semantic.Analyse_Class_Declaration (Node);
                  Ack.Errors.Record_Errors (Node);

                  declare
                     use Aquarius.Messages;
                     List : Message_List;
                  begin
                     Input.Get_Messages (List);
                     if Message_Count (List) = 0 or else
                       Highest_Level (List) <= Warning
                     then
                        Ack.Generate.Generate_Class_Declaration
                          (Node);
                     end if;
                  end;

               end;
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
                  Theme := Komnenos.Themes.Active_Theme;
               else
                  declare
                     Config : constant Tropos.Configuration :=
                                Aquarius.Configuration.Theme_Configuration;
                  begin
                     if Config.Contains (Command_Line.Theme) then
                        Theme :=
                          Komnenos.Themes.Load_Theme
                            (Config.Child (Command_Line.Theme));
                     else
                        Ada.Text_IO.Put_Line
                          (Ada.Text_IO.Standard_Error,
                           Command_Line.Theme
                           & ": unknown theme; using default");
                        Theme := Komnenos.Themes.Active_Theme;
                     end if;
                  end;
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
                    (Program          => Input,
                     Renderer         => Renderer);

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

         Aquarius.Plugins.Manager.Loaded_Plugin_Report;
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
                  Komnenos.UI.Create_UI;
         Load_Session : Boolean := True;
      begin

         UI.Load_Style_Sheet
           (Aquarius.Config_Paths.Config_File
              ("styles/default.css"));

--         Komnenos.Entities.Aqua_Entities.Create_Aqua_Object (UI);

         if Command_Line.Project_Name /= "" then
            declare
               Project : constant Aquarius.Projects.Aquarius_Project :=
                           Aquarius.Projects.Files.Load_Project_From_File
                             (Command_Line.Project_Name);
            begin
               Project.Write_Session_File
                 (".aquarius-session");
            end;
         elsif Command_Line.Plugin_Name /= "" then
            declare
               Name : constant String := Command_Line.Plugin_Name;
               Grammar   : constant Aquarius.Grammars.Aquarius_Grammar :=
                             Aquarius.Grammars.Manager.Get_Grammar
                               (Name);
            begin
               Aquarius.Grammars.UI.Load_Grammar
                 (Grammar, UI);
               Load_Session := False;
            end;
         end if;

         if Command_Line.Session_File /= "" then
            Komnenos.UI.Sessions.Load_Session
              (UI, Command_Line.Session_File);
         elsif Load_Session
           and then Ada.Directories.Exists (".aquarius-session")
         then
            Komnenos.UI.Sessions.Load_Session (UI, ".aquarius-session");
         end if;

         if Command_Line.Input_File /= ""
           or else Command_Line.Extra_Arguments /= ""
         then
            declare
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
            begin
               Grammar.Run_Action_Trigger
                 (Input,
                  Aquarius.Actions.Semantic_Trigger);
            end;
         end if;

         Show_Allocations;

         UI.Start;

         Show_Allocations;

         if Load_Session then
            Komnenos.UI.Sessions.Save_Session (UI, ".aquarius-session");
         end if;

      end;

   end if;

exception
   when E : others =>
      Ada.Text_IO.Put_Line
        ("exception: " & Ada.Exceptions.Exception_Name (E));
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Message (E));

end Aquarius.Driver;
