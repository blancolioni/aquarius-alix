with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Aquarius.Config_Paths;
with Aquarius.Errors;
with Aquarius.Programs.Parser;
with Aquarius.Projects;
with Aquarius.Source;
with Aquarius.Source.File_System;
with Aquarius.Syntax;
with Aquarius.Tokens;
with Aquarius.Transformers;
with Aquarius.Trees.Properties;

with Aquarius.Interaction.Console;
with Aquarius.UI.Console;

with Aquarius.Transformers.Action_Script;
pragma Unreferenced (Aquarius.Transformers.Action_Script);
--  just for the body elaboration

package body Aquarius.Loader is

   Trace_Files : constant Boolean := True;
   --  if True, report each loaded file to standard_output

   Show_Full_Path : constant Boolean := False;
   --  If true, the full path of each file is displayed while loading it

   function Load
     (Grammar    : in Aquarius.Grammars.Aquarius_Grammar;
      Project    : not null access Projects.Aquarius_Project_Type'Class;
      Interactor : not null access Aquarius.Interaction.Interactor'Class;
      UI         : Aquarius.UI.Aquarius_UI;
      Path       : in String)
     return Aquarius.Programs.Program_Tree;
   --  A support function to protect us from Name_Error

   procedure Get_Line
     (Line           : out String;
      Line_Last      : out Natural;
      Position       : in out Aquarius.Source.Source_Position;
      Grammar        : Aquarius.Grammars.Aquarius_Grammar;
      Vertical_Space : out Natural);

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (Line           : out String;
      Line_Last      : out Natural;
      Position       : in out Aquarius.Source.Source_Position;
      Grammar        : Aquarius.Grammars.Aquarius_Grammar;
      Vertical_Space : out Natural)
   is
      Line_First : Natural := 1;
   begin
      Vertical_Space := 0;
      loop
         Aquarius.Source.Get_Line
           (Position    => Position,
            Include_EOL => Grammar.Significant_End_Of_Line,
            Line        => Line,
            Last        => Line_Last);

         declare
            Trimmed_Line : constant String :=
                             Ada.Strings.Fixed.Trim
                               (Line (Line'First .. Line_Last),
                                Ada.Strings.Both);
         begin
            exit when Trimmed_Line'Length > 0
              or else Aquarius.Source.End_Of_File (Position);
         end;

         Aquarius.Source.Skip_Line (Position);
         Vertical_Space := Vertical_Space + 1;
      end loop;

      loop
         Line_First := Grammar.Line_Continues (Line (Line_First .. Line_Last));
         exit when Line_First = 0;
         Aquarius.Source.Skip_Line (Position);
         Aquarius.Source.Get_Line
           (Position, Grammar.Significant_End_Of_Line,
            Line (Line_First .. Line'Last), Line_Last);
      end loop;

   end Get_Line;

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
      Recovering : Boolean := False;
      Line_Comment : constant String :=
                       (if Grammar.Have_Line_Comment
                        then Grammar.Line_Comment
                        else "");
      Block_Comment_Start : constant String :=
                              (if Grammar.Have_Block_Comment
                               then Grammar.Block_Comment_Start
                               else "");
      Block_Comment_End : constant String :=
                              (if Grammar.Have_Block_Comment
                               then Grammar.Block_Comment_End
                               else "");

      Tok_Pos           : Aquarius.Source.Source_Position;

      function Token_OK (Tok : Aquarius.Tokens.Token) return Boolean
      is (Token_OK (Tok, Tok_Pos, Context));

   begin

      if Trace_Files then
         if Show_Full_Path then
            Ada.Text_IO.Put_Line ("Loading: " &
                                    Path);
         else
            Ada.Text_IO.Put_Line ("Loading: " &
                                    Ada.Directories.Simple_Name (Path));
         end if;
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
            Line           : String (1 .. 1000);
            Line_Last      : Natural;
            Next, First    : Natural;
            Old_First      : Natural;
            Class          : Aquarius.Tokens.Token_Class;
            Tok            : Aquarius.Tokens.Token;
            Complete       : Boolean;
            Unique         : Boolean;
            Have_Class     : Boolean;
            Have_Error     : Boolean;
            Vertical_Space : Natural := 0;
            LF : constant Character :=
                   Ada.Characters.Latin_1.LF;
         begin

            Get_Line (Line, Line_Last, Source_Pos, Grammar, Vertical_Space);

            exit when Aquarius.Source.End_Of_File (Source_Pos);

            Aquarius.Programs.Parser.Set_Vertical_Space (Context,
                                                         Vertical_Space);

            --  Ada.Text_IO.Put_Line (Line (Line'First .. Line_Last));

            Next    := Line'First;
            First   := Line'First;

            << Restart_Space_Scan >>

            while First <= Line_Last loop
               Old_First := First;
               Have_Error := False;

               --  don't try to parse remaining spaces on the line
               while First <= Line_Last
                 and then
                   (Ada.Characters.Handling.Is_Space (Line (First))
                    and then (not Grammar.Significant_End_Of_Line
                              or else Line (First) /= LF))
               loop
                  First := First + 1;
               end loop;

               exit when First > Line_Last;

               --  explicit check for line comment
               if Grammar.Have_Line_Comment then
                  if Line (First .. First + Line_Comment'Length - 1)
                    = Line_Comment
                  then
                     --  FIXME: save the text!

                     --  If end of line is significant, we have to treat
                     --  line comments as end of line; otherwise we simply
                     --  exit this loop which will take us to the next line.
                     if Grammar.Significant_End_Of_Line then
                        First := Line_Last;
                     else
                        exit;
                     end if;
                  end if;
               end if;

               --  explicit check for block comment
               if Grammar.Have_Block_Comment
                 and then First + Block_Comment_Start'Length - 1 <= Line_Last
                 and then Line
                   (First .. First + Block_Comment_Start'Length - 1)
                 = Block_Comment_Start
               then
                  First := First + Block_Comment_Start'Length;

                  declare
                     Found        : Boolean := False;
                  begin

                     while not Found loop

                        declare
                           Index : constant Natural :=
                                     Ada.Strings.Fixed.Index
                                       (Line (First .. Line_Last),
                                        Block_Comment_End);
                        begin
                           if Index > 0 then
                              First := Index + Block_Comment_End'Length;
                              Found := True;
                              exit;
                           end if;

                           Aquarius.Source.Skip_Line (Source_Pos);
                           Aquarius.Source.Get_Line
                             (Position    => Source_Pos,
                              Include_EOL =>
                                Grammar.Significant_End_Of_Line,
                              Line        => Line,
                              Last        => Line_Last);
                           First := Line'First;
                        end;
                     end loop;

                     goto Restart_Space_Scan;

                  end;
               end if;

               Aquarius.Tokens.Scan (Grammar.Frame, Line (1 .. Line_Last),
                                     False, Complete, Have_Class, Unique,
                                     Class, Tok, First, Next, Token_OK'Access);
               if Have_Class then
                  Tok_Pos := Aquarius.Source.Get_Column_Position
                    (Source_Pos, Aquarius.Source.Column_Number (First));
                  if Token_OK (Tok, Tok_Pos, Context) then
                     Recovering := False;
                     Parse_Token (Tok, Tok_Pos,
                                  Line (First .. Next), Context);
                  else
                     if not Recovering then
--                          Ada.Text_IO.Put_Line
--                            (Ada.Text_IO.Standard_Error,
--                             Line (1 .. Line_Last));
                        Ada.Text_IO.Put
                          (Ada.Text_IO.Standard_Error,
                           Aquarius.Source.Show (Tok_Pos) &
                             ": syntax error at " &
                             Line (First .. Next));

                        declare
                           use Aquarius.Tokens;
                           All_Terminals : constant Array_Of_Tokens :=
                                             Terminals (Grammar.Frame);
                        begin
                           Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
                                            " (expected");
                           for T of All_Terminals loop
                              if Token_OK (T, Tok_Pos, Context) then
                                 Ada.Text_IO.Put
                                   (Ada.Text_IO.Standard_Error,
                                    " " & Get_Name (Grammar.Frame, T));
                              end if;
                           end loop;
                           Ada.Text_IO.Put_Line
                             (Ada.Text_IO.Standard_Error, ")");
                        end;

                        Have_Error := True;
                        Recovering := True;
                     end if;
                  end if;

                  Aquarius.Programs.Parser.Set_Vertical_Space
                    (Context, 0);
                  Aquarius.Programs.Parser.Clear_Comments
                    (Context);

               else
                  Have_Error := True;
--                    Ada.Text_IO.Put_Line
--                      (Ada.Text_IO.Standard_Error,
--                       Line (1 .. Line_Last));
                  Aquarius.Errors.Error
                    (null, null,
                     Aquarius.Source.Show (Source_Pos)
                     & ": unable to determine class of token '"
                     & Line (Old_First .. Line_Last)
                     & "'");

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
      UI : constant Aquarius.UI.Aquarius_UI :=
             Aquarius.UI.Console.Console_UI;
   begin
      --  check first line of the file for special instructions
      declare
         use Ada.Text_IO;
         File : File_Type;
      begin
         Open (File, In_File, Path);
         declare
            Line : constant String := Get_Line (File);
         begin
            Close (File);
            if Line'Length > 10
              and then Line (Line'First .. Line'First + 10) = "@!aquarius:"
            then
               Ada.Text_IO.Put_Line
                 ("special: " & Line (Line'First + 11 .. Line'Last));
               declare
                  use Ada.Directories;
                  use Ada.Strings, Ada.Strings.Fixed;
                  use Aquarius.Transformers;
                  Transformer : constant Root_Transformer_Type'Class :=
                                  Get_Transformer
                                    (Trim
                                       (Line (Line'First + 11 .. Line'Last),
                                        Both));
                  Destination_Path : constant String :=
                                       Aquarius.Config_Paths.Config_Path
                                       & "/scratch/"
                                       & Simple_Name (Path);
               begin
                  Execute_Transformer (Transformer      => Transformer,
                                       Source_Path      => Path,
                                       Destination_Path => Destination_Path);
                  return Load_From_File
                    (Grammar    => Grammar,
                     Project    => Projects.New_Empty_Project (UI),
                     Interactor => Interaction.Console.Console_Interactor,
                     UI         => UI,
                     Path       => Destination_Path);
               end;
            end if;
         end;
      end;
      return Load_From_File
        (Grammar    => Grammar,
         Project    => Projects.New_Empty_Project (UI),
         Interactor => Interaction.Console.Console_Interactor,
         UI         => UI,
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
