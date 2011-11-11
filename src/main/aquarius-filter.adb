with Ada.Text_IO;

with Aquarius.Actions;
with Aquarius.Command_Line;
with Aquarius.Configuration;
with Aquarius.Grammars;
with Aquarius.Grammars.Manager;
with Aquarius.Loader;
with Aquarius.Messages.Console;
with Aquarius.Programs;                 use Aquarius.Programs;
with Aquarius.Programs.Arrangements;
with Aquarius.Rendering;
with Aquarius.Rendering.Manager;
with Aquarius.Styles;
with Aquarius.Target.Manager;
;
with Aquarius.Trees.Cursors;
with Aquarius.Version;

package body Aquarius.Filter is

procedure Aquarius.Consol_Main is

   procedure Show_Usage_Text;

   ---------------------
   -- Show_Usage_Text --
   ---------------------

   procedure Show_Usage_Text is
      use Ada.Text_IO;
      use type Aquarius.Trace.Debug_Class;
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
      Put ("Debug modes: ");
      for I in Aquarius.Trace.Debug_Class loop
         Put (Aquarius.Trace.Debug_Name (I));
         if I < Aquarius.Trace.Debug_Class'Last then
            Put (", ");
            if Col > 64 then
               New_Line;
               Put ("             ");
            end if;
         else
            New_Line;
         end if;
      end loop;

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

   Aquarius.Configuration.Load_Configuration;

   if Command_Line.Version then

      Ada.Text_IO.Put_Line (Version.Version_String);
      return;

   end if;

   if Command_Line.Help then
      Ada.Text_IO.Put_Line (Version.Version_String);
      Show_Usage_Text;
      return;
   end if;

   Aquarius.Target.Manager.Set_Target (Aquarius.Command_Line.Target);

   if Aquarius.Command_Line.Enable_Debug /= "" then
      begin
         Aquarius.Trace.Enable (Aquarius.Command_Line.Enable_Debug);
      exception
         when Constraint_Error =>
            return;
      end;

      Aquarius.Trace.Start_Trace;
   end if;

   if Aquarius.Command_Line.Filter then

      if Aquarius.Command_Line.Input_File = "" then
         Ada.Text_IO.Put_Line ("no input file specified");
         return;
      end if;

      declare
         use type Aquarius.Grammars.Aquarius_Grammar;
         Grammar     : Aquarius.Grammars.Aquarius_Grammar;
         Input       : Aquarius.Programs.Program_Tree;
         Renderer    : Aquarius.Rendering.Aquarius_Renderer;
         Style       : Aquarius.Styles.Aquarius_Style;
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

         Ada.Text_IO.Put_Line ("Loading " &
                                 Command_Line.Input_File);

         Input := Aquarius.Loader.Load_From_File (Grammar,
                                                  null, null,
                                                  Command_Line.Input_File);

         if Input = null then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  "Cannot load '" & Command_Line.Input_File &
                                    "'");
            return;
         end if;

         Ada.Text_IO.Put_Line ("done");

         declare
            use Aquarius.Messages;
            List : Message_List;
         begin
            Input.Get_Messages (List);
            if Message_Count (List) > 0 then
               if Highest_Level (List) > Warning then
                  Aquarius.Messages.Console.Show_Messages (List);
                  return;
               end if;
            end if;
         end;

         Ada.Text_IO.Put_Line ("no messages");

         if Command_Line.Action /= "" then
            Grammar.Run_Actions (Command_Line.Action, Input);
         else
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
                  return;
               end if;
            end if;
         end;

         Aquarius.Programs.Arrangements.Arrange (Input);

         if Command_Line.Renderer = "" then
            Renderer := Aquarius.Rendering.Manager.Load_Renderer ("text");
         else
            Renderer :=
              Aquarius.Rendering.Manager.Load_Renderer
              (Command_Line.Renderer);
         end if;

         if Command_Line.Style = "" then
            Style := Aquarius.Styles.Default_Style;
         else
            Style := Aquarius.Styles.Load_Style (Command_Line.Style);
         end if;

         Renderer.Set_Style (Style);

         declare
            use Ada.Text_IO;
            Using_File : constant Boolean := Command_Line.Output_File /= "";
            File : File_Type;
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

      exception
         when others =>
            if Input /= null then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     "Caught exception");
            end if;
            raise;
      end;

   else
      Aquarius.Editor.Start_Editor;
   end if;

   Aquarius.Trace.End_Trace;

exception
   when others =>
      Aquarius.Trace.End_Trace;
      raise;
end Aquarius.Main;
