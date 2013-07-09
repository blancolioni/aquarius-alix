with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Aquarius.Fragments.Text;
with Aquarius.Programs.Arrangements;
with Aquarius.Programs.Parser;
with Aquarius.Source;
with Aquarius.Tokens;
with Aquarius.Trees.Cursors;

package body Aquarius.Fragments.Programs is

   type Program_Fragment is
     new Aquarius.Fragments.Text.Text_Fragment with
      record
         Grammar  : Aquarius.Grammars.Aquarius_Grammar;
         Program  : Aquarius.Programs.Program_Tree;
         Context  : Aquarius.Programs.Parser.Parse_Context;
         Cursor   : Aquarius.Trees.Cursors.Cursor;
         Editing  : Boolean := False;
         Edit_Pos : Aquarius.Layout.Position;
         File_Pos : Aquarius.Source.Source_Position;
         Buffer   : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding
   procedure Render (Fragment : in out Program_Fragment);

   overriding
   function On_Key_Press (Fragment : in out Program_Fragment;
                          Position : in     Aquarius.Layout.Position;
                          Key      : in     Aquarius.Keys.Aquarius_Key)
                          return Boolean;

   function Scan_Token
     (Fragment : in out Program_Fragment'Class;
      Force    : in     Boolean)
      return Boolean;

   --------------------
   -- Create_Program --
   --------------------

   function Create_Program
     (Width, Height : Positive;
      Grammar       : Aquarius.Grammars.Aquarius_Grammar;
      Program       : Aquarius.Programs.Program_Tree)
      return Aquarius_Fragment
   is
      use Aquarius.Fragments.Text;
      Result : constant Aquarius_Fragment := new Program_Fragment;
   begin
      Program_Fragment (Result.all).Program := Program;
      Program_Fragment (Result.all).Grammar := Grammar;
      Program_Fragment (Result.all).Cursor  :=
        Aquarius.Trees.Cursors.Left_Of_Tree (Program);
      Result.Background := Aquarius.Fonts.White;

      Aquarius.Programs.Parser.Initialise_Parse_Context
        (Context     => Program_Fragment (Result.all).Context,
         Grammar     => Grammar,
         Root        => Program,
         Interactive => True);

      Result.Initialise (Width, Height);
      return Result;
   end Create_Program;

   ------------------
   -- On_Key_Press --
   ------------------

   overriding
   function On_Key_Press (Fragment : in out Program_Fragment;
                          Position : in     Aquarius.Layout.Position;
                          Key      : in     Aquarius.Keys.Aquarius_Key)
                          return Boolean
   is
      use Ada.Strings.Unbounded;
      use type Aquarius.Layout.Position;
      Result : Boolean := False;
   begin

      if Aquarius.Keys.Is_Character (Key) then
         declare
            Ch : constant Character := Aquarius.Keys.Get_Character (Key);
         begin

            if Fragment.Editing then
               if Fragment.Edit_Pos = Position then
                  Fragment.Buffer := Fragment.Buffer & Ch;
                  Result := Fragment.Scan_Token (Force => False);
               else
                  Result := Fragment.Scan_Token (Force => True);
               end if;
            else
               Fragment.Editing := True;
               declare
                  P : constant Aquarius.Programs.Program_Tree :=
                        Fragment.Program.Find_Node_At (Position);
               begin
                  Fragment.Cursor :=
                    Aquarius.Trees.Cursors.Left_Of_Tree (P);
                  Fragment.Edit_Pos := Position;
                  Ada.Text_IO.Put_Line ("Starting edit at "
                                        & Aquarius.Trees.Cursors.Image
                                          (Fragment.Cursor));
                  Aquarius.Layout.Next (Fragment.Edit_Pos);
                  Fragment.Buffer := To_Unbounded_String ((1 => Ch));
                  Result := False;
               end;
            end if;
         end;
      else
         Result := True;
      end if;

      return Result;

   end On_Key_Press;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Fragment : in out Program_Fragment)
   is
      use Ada.Strings.Unbounded;
   begin
      Aquarius.Programs.Arrangements.Arrange
        (Item           => Fragment.Program,
         Point          => Fragment.Cursor,
         Partial_Length => Length (Fragment.Buffer),
         Line_Length    => Fragment.Width / 8);

      Aquarius.Programs.Arrangements.Render
        (Program  => Fragment.Program,
         Renderer => Fragment.Renderer,
         Point    => Fragment.Cursor,
         Partial  => To_String (Fragment.Buffer));

   end Render;

   ----------------
   -- Scan_Token --
   ----------------

   function Scan_Token
     (Fragment : in out Program_Fragment'Class;
      Force    : in     Boolean)
      return Boolean
   is
      use Ada.Strings.Unbounded;
      Complete       : Boolean;
      Have_Class     : Boolean;
      Tok            : Aquarius.Tokens.Token;
      Class          : Aquarius.Tokens.Token_Class;
      First, Next    : Natural := 1;
      Buffer         : String := To_String (Fragment.Buffer);
      Length         : Natural := Buffer'Length;
      Got_Token      : Boolean := False;
      Need_Render    : Boolean := False;
   begin

      while Length > 0 loop

         Aquarius.Tokens.Scan (Fragment.Grammar.Frame,
                               Buffer (1 .. Length),
                               False, Complete, Have_Class,
                               Class, Tok, First, Next);

         if (Force and then Next > 0) or else
           (Have_Class and then Complete and then Next < Length)
         then
            Ada.Text_IO.Put_Line
              ("Token: "
               & Aquarius.Tokens.Get_Token_Class_Name
                 (Fragment.Grammar.Frame, Tok)
               & ": "
               & Buffer (First .. Next));

            declare
               use Aquarius.Programs.Parser;
            begin
               if Token_OK (Tok, Fragment.File_Pos, Fragment.Context) then
                  Parse_Token (Tok, Fragment.File_Pos,
                               Buffer (First .. Next), Fragment.Context);
                  Need_Render := True;
               else
                  Ada.Text_IO.Put_Line ("Cancelled");
                  return False;
               end if;
            end;

            Buffer (1 .. Length - Next) := Buffer (Next + 1 .. Length);
            Length := Length - Next;
            Got_Token := True;
         else
            exit;
         end if;

      end loop;

      if Got_Token then
         Fragment.Buffer := To_Unbounded_String (Buffer (1 .. Length));
      end if;

      if Need_Render then
         Fragment.Render;
      else
         Aquarius.Layout.Next (Fragment.Edit_Pos);
      end if;

      return False;
   end Scan_Token;

end Aquarius.Fragments.Programs;
