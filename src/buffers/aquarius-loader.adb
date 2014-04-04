with System.Assertions;

with Ada.Directories;
with Ada.Text_IO;

with Aquarius.Programs.Parser;
with Aquarius.Projects;
with Aquarius.Source;
with Aquarius.Source.File_System;
with Aquarius.Syntax;
with Aquarius.Tokens;
with Aquarius.Trees.Cursors;
with Aquarius.Trees.Properties;

with Aquarius.Interaction.Console;
with Aquarius.UI.Console;

package body Aquarius.Loader is

   Show_Full_Path : constant Boolean := True;
   --  If true, the full path of each file is displaed while loading it

   function Load
     (Grammar    : in Aquarius.Grammars.Aquarius_Grammar;
      Project    : not null access Projects.Aquarius_Project_Type'Class;
      Interactor : not null access Aquarius.Interaction.Interactor'Class;
      UI         : Aquarius.UI.Aquarius_UI;
      Path       : in String)
     return Aquarius.Programs.Program_Tree;
   --  A support function to protect us from Name_Error

   ----------
   -- Load --
   ----------

   function Load
     (Grammar    : in     Aquarius.Grammars.Aquarius_Grammar;
      Project    : not null access Projects.Aquarius_Project_Type'Class;
      Interactor : not null access Aquarius.Interaction.Interactor'Class;
      UI         : Aquarius.UI.Aquarius_UI;
      Path       : in     String)
     return Aquarius.Programs.Program_Tree
   is
      use Aquarius.Programs, Aquarius.Programs.Parser;
      Defn   : constant Aquarius.Syntax.Syntax_Tree :=
                 Grammar.Get_Top_Level_Syntax;
      pragma Assert (Aquarius.Syntax."/=" (Defn, null));
      File : constant Aquarius.Source.Source_File :=
        Aquarius.Source.File_System.Read_File (Path);
      Result : constant Aquarius.Programs.Program_Tree :=
        Aquarius.Programs.New_Program (Defn, File);
      Source_Pos : Aquarius.Source.Source_Position :=
        Aquarius.Source.Get_Start (File);
      Context    : Parse_Context;
   begin

      if Show_Full_Path then
         Ada.Text_IO.Put_Line ("Loading: " &
                               Path);
      else
         Ada.Text_IO.Put_Line ("Loading: " &
                               Ada.Directories.Simple_Name (Path));
      end if;

      Aquarius.Trees.Properties.Set_Grammar (Result.all, Grammar);
      Aquarius.Trees.Properties.Set_Interactor (Result.all, Interactor);
      Aquarius.Trees.Properties.Set_UI (Result.all, UI);
      Aquarius.Trees.Properties.Set_Project
        (Result.all,
         Aquarius.Projects.Aquarius_Project (Project));

      Initialise_Parse_Context (Context, Grammar, Result,
                                Interactive => False);

      while not Aquarius.Source.End_Of_File (Source_Pos) loop
         declare
            use type Aquarius.Tokens.Token;
            Line              : String (1 .. 1000);
            Line_Last         : Natural;
            Next, First       : Natural;
            Class             : Aquarius.Tokens.Token_Class;
            Tok               : Aquarius.Tokens.Token;
            Tok_Pos           : Aquarius.Source.Source_Position;
            Complete          : Boolean;
            Have_Class        : Boolean;
            Have_Error        : Boolean;
            Vertical_Space    : Natural := 0;
         begin
            loop
               Aquarius.Source.Get_Line (Source_Pos, Line, Line_Last);
               exit when Line_Last > 0 or else
                 Aquarius.Source.End_Of_File (Source_Pos);
               Aquarius.Source.Skip_Line (Source_Pos);
               Vertical_Space := Vertical_Space + 1;
            end loop;

            exit when Aquarius.Source.End_Of_File (Source_Pos);

            Aquarius.Programs.Parser.Set_Vertical_Space (Context,
                                                         Vertical_Space);

            --  Ada.Text_IO.Put_Line (Line (Line'First .. Line_Last));

            Next    := Line'First;
            First   := Line'First;
            while First <= Line_Last loop
               Have_Error := False;

               Aquarius.Tokens.Scan (Grammar.Frame, Line (1 .. Line_Last),
                                     False, Complete, Have_Class,
                                     Class, Tok, First, Next);
               if Have_Class then
                  Tok_Pos := Aquarius.Source.Get_Column_Position
                    (Source_Pos, Aquarius.Source.Column_Number (First));
                  if Tok = Grammar.Comment_Token then
                     Add_Comment
                       (Context,
                        Grammar.Make_Comment_Tree (Line (First .. Next)));
                  elsif Token_OK (Tok, Tok_Pos, Context) then
                     Parse_Token (Tok, Tok_Pos,
                                  Line (First .. Next), Context);
                  else
                     Ada.Text_IO.Put_Line (Aquarius.Source.Show (Tok_Pos) &
                                           ": syntax error at " &
                                           Line (First .. Next));
                     declare
                        use Aquarius.Trees.Cursors;
                        It : Cursor := Get_Cursor (Context);
                     begin
                        while not Is_At_Root (It) loop
                           Ada.Text_IO.Put_Line
                             (Aquarius.Trees.Cursors.Image (It));
                           Move_To_Left_Of_Parent (It);
                        end loop;
                        return Result;
                     end;
                  end if;
               else
                  Have_Error := True;
                  --  Next is set to zero by the token scanner
                  --  if we don't get a class back.
                  Next := First;
               end if;

               if Have_Error then
                  Add_Error (Context,
                             Grammar.Make_Error_Tree
                               (Tok_Pos, Line (First .. Next)));
               end if;

               First := Next + 1;

            end loop;

            Aquarius.Source.Skip_Line (Source_Pos);

         end;

      end loop;

      Finish_Parse (Context);

      return Result;

   exception
      when System.Assertions.Assert_Failure =>

         if Result /= null then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  "assert failure");
         end if;
         raise;

   end Load;

   --------------------
   -- Load_From_File --
   --------------------

   function Load_From_File
     (Grammar    : in     Aquarius.Grammars.Aquarius_Grammar;
      Project    : not null access Programs.Root_Program_Tree_Store'Class;
      Interactor : not null access Interaction.Interactor'Class;
      UI         : Aquarius.UI.Aquarius_UI;
      Path       : in     String)
     return Aquarius.Programs.Program_Tree
   is
   begin
      --  type conversion to project will be fixed once we
      --  change the project property to a Program_Store one
      return Load (Grammar,
                   Aquarius.Projects.Aquarius_Project (Project),
                   Interactor, UI, Path);

   exception

      when Ada.Text_IO.Name_Error =>
         Ada.Text_IO.Put_Line (Path & ": file not found");
         return null;

   end Load_From_File;

   --------------------
   -- Load_From_File --
   --------------------

   function Load_From_File
     (Grammar    : in     Aquarius.Grammars.Aquarius_Grammar;
      Path       : in     String)
     return Aquarius.Programs.Program_Tree
   is
   begin
      return Load_From_File
        (Grammar    => Grammar,
         Project    => Projects.New_Empty_Project,
         Interactor => Interaction.Console.Console_Interactor,
         UI         => Aquarius.UI.Console.Console_UI,
         Path       => Path);
   end Load_From_File;

   --------------------
   -- Load_From_File --
   --------------------

   function Load_From_File
     (Grammar    : in     Aquarius.Grammars.Aquarius_Grammar;
      Project    : not null access Programs.Root_Program_Tree_Store'Class;
      UI         : Aquarius.UI.Aquarius_UI;
      Path       : in     String)
     return Aquarius.Programs.Program_Tree
   is
   begin
      return Load_From_File
        (Grammar    => Grammar,
         Project    => Project,
         Interactor => Interaction.Console.Console_Interactor,
         UI         => UI,
         Path       => Path);
   end Load_From_File;

end Aquarius.Loader;
