with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Aquarius.Programs.Editor is

   function Contains_Position
     (Input : Input_Buffer;
      Position : Aquarius.Layout.Position)
      return Boolean;

   procedure Set_Cursor
     (Input        : in out Input_Buffer;
      From_Position : Aquarius.Layout.Position);

   procedure Complete_Edit
     (Editor : in out Root_Program_Editor'Class);

   procedure Check_Token
     (Editor : in out Root_Program_Editor'Class;
      Force  : in     Boolean;
      Echo   :    out Boolean);

   procedure Scan_Token
     (Editor : in out Root_Program_Editor'Class;
      Force  : in     Boolean;
      Length :    out Natural;
      Tok    :    out Aquarius.Tokens.Token);

   procedure Insert_Character
     (Input : in out Input_Buffer;
      Ch    : Character);

   procedure Delete_Range
     (Input         : in out Input_Buffer;
      Start, Finish : Positive);

   procedure Start_Edit
     (Input        : in out Input_Buffer;
      Position     : Aquarius.Layout.Position;
      Start_Text   : String;
      Start_Cursor : Natural);

   ------------------
   -- Add_Renderer --
   ------------------

   procedure Add_Renderer
     (Editor   : in out Root_Program_Editor;
      Renderer : access Aquarius_Rendering_Interface'Class;
      Top      : Program_Tree)
   is
   begin
      Editor.Renderer := Renderer;
      Editor.Top      := Top;
      Ada.Text_IO.Put_Line
        ("Editor: set renderer: top = " & Top.Image);
   end Add_Renderer;

   ---------------
   -- Backspace --
   ---------------

   procedure Backspace
     (Editor : in out Root_Program_Editor'Class)
   is
   begin
      Editor.Delete_Characters (-1, -1);
   end Backspace;

   -----------------
   -- Check_Token --
   -----------------

   procedure Check_Token
     (Editor : in out Root_Program_Editor'Class;
      Force  : in     Boolean;
      Echo   :    out Boolean)
   is
      use Aquarius.Programs.Parser;
      Tok       : Aquarius.Tokens.Token;
      Length    : Natural;
      Got_Token : Boolean := False;
      Start     : constant Natural :=
        Ada.Strings.Fixed.Index_Non_Blank
        (Editor.Input.Buffer (1 .. Editor.Input.Length));
   begin
      if Start = 0 then
         Editor.Input.Length := 0;
         Echo := True;
         return;
      end if;

      Echo := True;

      while Editor.Input.Length > 0 loop
         Ada.Text_IO.Put_Line
           ("Check_Token ["
            & Editor.Input.Buffer (1 .. Editor.Input.Length)
            & "]");
         Scan_Token (Editor, Force, Length, Tok);
         exit when Length = 0;

         declare
            use Aquarius.Source;
         begin
            Set_Position (Editor.Source_Pos,
                          Line_Number (Editor.Position.Line),
                          Column_Number (Editor.Position.Column));
         end;

         declare
            use type Aquarius.Tokens.Token;
            Text : constant String :=
                     Editor.Input.Buffer (1 .. Length);
         begin

            Ada.Text_IO.Put_Line ("got token [" & Text & "]");

            if Editor.Edit_Terminal /= null then
               if Tok = Editor.Edit_Terminal.Get_Token then
                  --  replace the text of the existing token
                  Editor.Edit_Terminal.Fill (Text);
                  Editor.Edit_Terminal := null;
                  --  TODO: update semantics
               else
                  --  we edited a token and changed it to something else
                  --  right now we're fucked
                  null;
               end if;
            else
               --  creating a new token
               Ada.Text_IO.Put_Line
                 ("Parsing into: "
                  & Aquarius.Trees.Cursors.Image
                    (Aquarius.Programs.Parser.Get_Cursor
                       (Editor.Context)));

               if Token_OK (Tok, Editor.Source_Pos, Editor.Context) then
                  Parse_Token (Tok, Editor.Source_Pos, Text, Editor.Context);
                  Ada.Text_IO.Put_Line
                    ("After parse: "
                     & Aquarius.Trees.Cursors.Image
                       (Aquarius.Programs.Parser.Get_Cursor
                          (Editor.Context)));
                  Got_Token := True;
               else
                  Editor.Input.Length := Editor.Input.Length - 1;
                  Echo := False;
                  exit;
               end if;
            end if;

            --  removed the processed text from our buffer
            Delete_Range (Editor.Input, 1, Length);
         end;
      end loop;

      if Got_Token and then
        not Aquarius.Programs.Parser.Is_Ambiguous
          (Editor.Context)
      then
         Editor.Renderer.Update
           (Point   => Aquarius.Programs.Parser.Get_Cursor (Editor.Context),
            Partial => Editor.Input.Buffer (1 .. Editor.Input.Length));
         Echo := False;
      end if;

   end Check_Token;

   -------------------
   -- Complete_Edit --
   -------------------

   procedure Complete_Edit
     (Editor : in out Root_Program_Editor'Class)
   is
      Echo : Boolean;
   begin
      Check_Token (Editor, True, Echo);
   end Complete_Edit;

   -----------------------
   -- Contains_Position --
   -----------------------

   function Contains_Position
     (Input : Input_Buffer;
      Position : Aquarius.Layout.Position)
      return Boolean
   is
      use type Aquarius.Layout.Count;
      Start_Column : constant Aquarius.Layout.Positive_Count :=
                       Input.Start.Column;
      End_Column   : constant Aquarius.Layout.Count :=
                       Input.Start.Column
                         + Aquarius.Layout.Count (Input.Length) - 1;
   begin
      return Input.Start.Line = Position.Line
        and then Position.Column in Start_Column .. End_Column;
   end Contains_Position;

   -------------------
   -- Create_Editor --
   -------------------

   function Create_Editor
     (Root : Program_Tree)
      return Program_Editor
   is
   begin
      return Editor : constant Program_Editor := new Root_Program_Editor do
         Editor.Root := Root;
         Editor.Top  := Root;
         Editor.Left_Terminal := null;
         Editor.Right_Terminal := Program_Tree (Root.First_Leaf);
         Editor.Renderer := null;
         Aquarius.Programs.Parser.Initialise_Parse_Context
           (Context     => Editor.Context,
            Grammar     => null,
            Root        => Editor.Root,
            Interactive => True,
            Run_Actions => False);
      end return;
   end Create_Editor;

   ----------------------
   -- Delete_Character --
   ----------------------

   procedure Delete_Characters
     (Editor : in out Root_Program_Editor;
      Start  : Integer;
      Finish : Integer)
   is
      Left  : constant Integer := Editor.Input.Cursor + Start + 1;
      Right : constant Integer := Editor.Input.Cursor + Finish + 1;
   begin
      if Left in 1 .. Editor.Input.Length
        and then Right in 1 .. Editor.Input.Length
      then
         declare
            New_Text : constant String :=
                         Editor.Input.Buffer (1 .. Left - 1)
                       & Editor.Input.Buffer
                                (Right + 1 .. Editor.Input.Length);
         begin
            Editor.Input.Buffer (1 .. New_Text'Length) := New_Text;
            Editor.Input.Length := New_Text'Length;
            if Start < 0 then
               Editor.Input.Cursor := Editor.Input.Cursor + Start;
            end if;
            Ada.Text_IO.Put_Line
              ("Editor: delete characters: new text = ["
               & Editor.Input.Buffer (1 .. Editor.Input.Length)
               & "]");
         end;
      else
         --  TODO: if we delete past the beginning of the buffer,
         --  we should activate the previous terminal and delete
         --  characters from that.  Similarly for deleting past the
         --  end of the buffer;
         null;
      end if;
   end Delete_Characters;

   ------------------
   -- Delete_Range --
   ------------------

   procedure Delete_Range
     (Input         : in out Input_Buffer;
      Start, Finish : Positive)
   is
      Length : constant Natural := Finish - Start + 1;
   begin
      Input.Buffer (Start .. Input.Length - Length) :=
        Input.Buffer (Start + Length .. Input.Length);
      Input.Length := Input.Length - Length;
      if Input.Cursor in Start .. Finish then
         Input.Cursor := Start;
      elsif Input.Cursor > Finish then
         Input.Cursor :=
           Input.Cursor - Length;
      end if;

      Ada.Text_IO.Put_Line
        ("Editor: delete range: " & Start'Img & " .." & Finish'Img
         & ": new text = [" & Input.Buffer (1 .. Input.Length)
         & "]");

   end Delete_Range;

   ----------------------
   -- Insert_Character --
   ----------------------

   procedure Insert_Character
     (Editor : in out Root_Program_Editor;
      Ch     : Character;
      Echo   : out Boolean)
   is
   begin
      Ada.Text_IO.Put_Line
        ("Editor: insert character: " & Ch);

      if not Editor.Input.Active then
         if Editor.Left_Terminal /= null
           and then Editor.Join_Left
         then
            declare
               Text : constant String :=
                        Editor.Left_Terminal.Text & Ch;
               Complete : Boolean;
               Have_Class : Boolean;
               Class      : Aquarius.Tokens.Token_Class;
               Tok        : Aquarius.Tokens.Token;
               First      : Positive := 1;
               Last       : Natural;
            begin
               Aquarius.Tokens.Scan
                 (Frame      => Editor.Left_Terminal.Syntax.Frame,
                  Text       => Text,
                  Partial    => False,
                  Complete   => Complete,
                  Have_Class => Have_Class,
                  Class      => Class,
                  Tok        => Tok,
                  First      => First,
                  Last       => Last);
               if Last = Text'Last then
                  --  we can join the previous token
                  Ada.Text_IO.Put_Line
                    ("Editor: join left terminal "
                     & Editor.Left_Terminal.Text);

                  Editor.Edit_Terminal := Editor.Left_Terminal;
                  Start_Edit (Editor.Input,
                              Editor.Left_Terminal.Layout_Start_Position,
                              Editor.Edit_Terminal.Text,
                              Last - 1);
               end if;
            end;
         end if;
      end if;

      if not Editor.Input.Active then
         Editor.Edit_Terminal := null;
         Start_Edit (Editor.Input, Editor.Position, "", 0);
      end if;

      Insert_Character (Editor.Input, Ch);

      Check_Token (Editor, False, Echo);

   end Insert_Character;

   ----------------------
   -- Insert_Character --
   ----------------------

   procedure Insert_Character
     (Input : in out Input_Buffer;
      Ch    : Character)
   is
   begin
      Input.Cursor := Input.Cursor + 1;
      Input.Buffer (Input.Cursor + 1 .. Input.Length + 1) :=
        Input.Buffer (Input.Cursor .. Input.Length);
      Input.Buffer (Input.Cursor) := Ch;
      Input.Length := Input.Length + 1;
   end Insert_Character;

   ----------------
   -- Scan_Token --
   ----------------

   procedure Scan_Token
     (Editor : in out Root_Program_Editor'Class;
      Force  : in     Boolean;
      Length :    out Natural;
      Tok    :    out Aquarius.Tokens.Token)
   is
      Complete    : Boolean;
      Have_Class  : Boolean;
      Class       : Aquarius.Tokens.Token_Class;
      First, Next : Natural := 1;
   begin
      Aquarius.Tokens.Scan
        (Editor.Root.Syntax.Frame,
         Editor.Input.Buffer (1 .. Editor.Input.Length),
         False, Complete, Have_Class,
         Class, Tok, First, Next);
      if (Force and then Next > 0) or else
        (Have_Class and then Complete and then Next < Editor.Input.Length)
      then
         Length := Next;
      else
         Length := 0;
      end if;

   end Scan_Token;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor
     (Input        : in out Input_Buffer;
      From_Position : Aquarius.Layout.Position)
   is
   begin
      Input.Cursor :=
        Positive (From_Position.Column)
        - Positive (Input.Start.Column);
   end Set_Cursor;

   ---------------
   -- Set_Point --
   ---------------

   procedure Set_Point
     (Editor : in out Root_Program_Editor;
      Point  : in     Aquarius.Layout.Position)
   is
      use Aquarius.Layout;
   begin
      Editor.Position := Point;
      Ada.Text_IO.Put_Line
        ("Editor: set point: " & Aquarius.Layout.Show (Editor.Position));

      if Editor.Input.Active
        and then Contains_Position (Editor.Input, Point)
      then
         --  we are remaining within the current edit,
         --  so don't commit anything
         Set_Cursor (Editor.Input, Point);
      else
         --  if changes have been made, commit them before continuing
         if Editor.Input.Active
           and then Editor.Input.Changed
         then
            Complete_Edit (Editor);
         end if;
         Editor.Left_Terminal  := Editor.Top.Find_Node_At (Point);

         declare
            New_Cursor : Aquarius.Trees.Cursors.Cursor;
         begin
            if Editor.Left_Terminal = null then
               Editor.Right_Terminal :=
                 Program_Tree (Editor.Top.First_Leaf);
               New_Cursor :=
                 Aquarius.Trees.Cursors.Left_Of_Tree (Editor.Right_Terminal);
            else
               Editor.Right_Terminal :=
                 Scan_Terminal (Editor.Left_Terminal, 1);
               New_Cursor :=
                 Aquarius.Trees.Cursors.Right_Of_Tree (Editor.Left_Terminal);
            end if;
            Aquarius.Programs.Parser.Set_Cursor
              (Editor.Context, New_Cursor);
         end;

      end if;
   end Set_Point;

   ----------------
   -- Start_Edit --
   ----------------

   procedure Start_Edit
     (Input        : in out Input_Buffer;
      Position     : Aquarius.Layout.Position;
      Start_Text   : String;
      Start_Cursor : Natural)
   is
   begin
      Input.Active := True;
      Input.Start := Position;
      Input.Buffer (1 .. Start_Text'Length) := Start_Text;
      Input.Length := Start_Text'Length;
      Input.Cursor := Start_Cursor;
      Input.Changed := False;
   end Start_Edit;

end Aquarius.Programs.Editor;
