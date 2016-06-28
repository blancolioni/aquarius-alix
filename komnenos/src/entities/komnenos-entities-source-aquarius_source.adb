with Ada.Text_IO;

with Aquarius.Grammars;
with Aquarius.Source;
with Aquarius.Tokens;
with Aquarius.Trees.Properties;

with Aquarius.Programs.Arrangements;
with Aquarius.Programs.Parser;

with Aquarius.Rendering;
with Aquarius.Themes;
with Aquarius.Trees.Cursors;

with Komnenos.Entities.Visuals;
with Komnenos.Fragments.Rendering;
with Komnenos.Fragments.Source;
with Komnenos.UI;

package body Komnenos.Entities.Source.Aquarius_Source is

   type Source_Position is
      record
         Tree   : Aquarius.Programs.Program_Tree;
         Offset : Aquarius.Layout.Position_Offset;
      end record;

   type Source_Cursor_Array is array (Cursor_Type) of Source_Position;

   type Root_Aquarius_Source_Entity is
     new Root_Source_Entity_Reference with
      record
         Top_Level              : Boolean;
         Changed                : Boolean;
         Compilation_Unit       : Aquarius.Programs.Program_Tree;
         Grammar                : Aquarius.Grammars.Aquarius_Grammar;
         Entity_Spec            : Aquarius.Programs.Program_Tree;
         Entity_Body            : Aquarius.Programs.Program_Tree;
         Entity_Tree            : Aquarius.Programs.Program_Tree;
         Update_Tree            : Aquarius.Programs.Program_Tree;
         Cursors                : Source_Cursor_Array :=
                                    (others => (null, 0));
         Edit_Buffer            : Ada.Strings.Unbounded.Unbounded_String;
         Parse_Context          : Aquarius.Programs.Parser.Parse_Context;
         Buffer_Changed         : Boolean := False;
         Invalidated            : Boolean := False;
         New_Line_Before_Buffer : Boolean := False;
      end record;

   overriding function Top_Level
     (Entity : Root_Aquarius_Source_Entity)
      return Boolean
   is (Entity.Top_Level);

   overriding procedure Select_Entity
     (Entity : not null access Root_Aquarius_Source_Entity;
      Table  : access Entity_Table_Interface'Class;
      Parent : access Entity_Visual'Class;
      Visual : access Entity_Visual'Class;
      Offset : Natural);

   overriding procedure Render
     (Entity : not null access Root_Aquarius_Source_Entity;
      Visual : not null access Entity_Visual'Class);

   overriding function Get_Cursor
     (Entity : Root_Aquarius_Source_Entity;
      Cursor : Cursor_Type)
      return Aquarius.Layout.Position;

   overriding procedure Set_Cursor
     (Entity   : in out Root_Aquarius_Source_Entity;
      Cursor   : Cursor_Type;
      Position : Aquarius.Layout.Position);

   overriding function Get_Line
     (Entity   : Root_Aquarius_Source_Entity;
      Position : Aquarius.Layout.Position)
      return Aquarius.Layout.Line_Number;

   overriding function Get_Column
     (Entity   : Root_Aquarius_Source_Entity;
      Position : Aquarius.Layout.Position)
      return Aquarius.Layout.Column_Number;

   overriding function Get_Start_Of_Line
     (Entity : Root_Aquarius_Source_Entity;
      Line   : Aquarius.Layout.Line_Number)
      return Aquarius.Layout.Position;

   overriding procedure Move_Cursor
     (Item     : in out Root_Aquarius_Source_Entity;
      Cursor   : Cursor_Type;
      Movement : Cursor_Movement_Type;
      Offset   : Aquarius.Layout.Position_Offset);

   overriding procedure Insert_Text
     (Item     : in out Root_Aquarius_Source_Entity;
      Text     : String);

   overriding procedure Delete_Region
     (Item     : in out Root_Aquarius_Source_Entity)
   is null;

   overriding function Get_Region_Text
     (Entity : Root_Aquarius_Source_Entity;
      End_1  : Cursor_Type;
      End_2  : Cursor_Type)
      return String
   is ("");

   procedure Forward_Character
     (Item   : in out Root_Aquarius_Source_Entity'Class;
      Cursor : Cursor_Type);

   procedure Backward_Character
     (Item   : in out Root_Aquarius_Source_Entity'Class;
      Cursor : Cursor_Type);

   procedure Insert_Character
     (Entity : in out Root_Aquarius_Source_Entity'Class;
      Ch     : Character);

   procedure Insert_New_Line
     (Item : in out Root_Aquarius_Source_Entity'Class);

   --     overriding procedure Execute_Command
--       (Item    : not null access Root_Aquarius_Source_Entity;
--        Command : Komnenos.Commands.Komnenos_Command);

   procedure Finish_Edit
     (Item : not null access Root_Aquarius_Source_Entity'Class)
   is null;

   procedure Check_Token
     (Entity : not null access Root_Aquarius_Source_Entity'Class;
      Force  : in     Boolean;
      Echo   : out Boolean);

   procedure Scan_Token
     (Entity : not null access Root_Aquarius_Source_Entity'Class;
      Force  : in     Boolean;
      Length :    out Natural;
      Tok    :    out Aquarius.Tokens.Token);

--     overriding procedure Insert_Character
--       (Item    : in out Root_Aquarius_Source_Entity;
--        Value   : Character;
--        Updated : out Boolean);

   function Get_Key (File_Name : String;
                     Line      : Natural;
                     Column    : Natural;
                     Name      : String)
                     return String;

   procedure Log_Tree (Tree : Aquarius.Programs.Program_Tree)
     with Unreferenced;

   ------------------------
   -- Backward_Character --
   ------------------------

   procedure Backward_Character
     (Item   : in out Root_Aquarius_Source_Entity'Class;
      Cursor : Cursor_Type)
   is
      use Ada.Strings.Unbounded;
      use Aquarius.Layout;
      Tree   : Aquarius.Programs.Program_Tree renames
                 Item.Cursors (Cursor).Tree;
      Offset : Position_Offset renames
                 Item.Cursors (Cursor).Offset;
   begin
      if Offset > 0 then
         Offset := Offset - 1;
         declare
            New_Position : constant Position :=
                             Tree.Layout_Start_Position
                               + Offset;
         begin
            Item.Set_Cursor (Cursor, New_Position);
            Komnenos.Entities.Visuals.Update_Cursor
              (Item, Cursor, New_Position);
         end;
      else
         if Item.Buffer_Changed then
            Item.Finish_Edit;
         end if;

         declare
            use Aquarius.Programs;
            New_Position  : Position := Tree.Layout_Start_Position;
            Next_Terminal : constant Program_Tree :=
                              Tree.Scan_Terminal (-1);
         begin
            if Next_Terminal = null or else not Next_Terminal.Is_Terminal then
               Offset := 0;
               New_Position := Tree.Layout_Start_Position;
            elsif Next_Terminal.Layout_Line < Tree.Layout_Line then
               New_Position := Next_Terminal.Layout_End_Position;
            else
               New_Position := Tree.Layout_Start_Position - 1;
            end if;
            Item.Set_Cursor (Cursor, New_Position);
            Komnenos.Entities.Visuals.Update_Cursor
              (Item, Cursor, New_Position);
         end;
      end if;
   end Backward_Character;

   -----------------
   -- Check_Token --
   -----------------

   procedure Check_Token
     (Entity : not null access Root_Aquarius_Source_Entity'Class;
      Force  : in     Boolean;
      Echo   : out Boolean)
   is
      use Ada.Strings.Unbounded;
      use Aquarius.Programs;
      use Aquarius.Programs.Parser;
      use Aquarius.Layout;

      Tok       : Aquarius.Tokens.Token;
      Last      : Natural;
      Got_Token : Boolean := False;
      Start     : Natural :=
                    Index_Non_Blank (Entity.Edit_Buffer);
      Offset    : Aquarius.Layout.Position_Offset renames
                    Entity.Cursors (Point).Offset;
   begin
      if Start = 0 then
--           Entity.Edit_Buffer := Null_Unbounded_String;
         Echo := True;
         return;
      end if;

      Echo := True;

      while Length (Entity.Edit_Buffer) > 0 loop
         Scan_Token (Entity, Force, Last, Tok);
         exit when Last = 0;

         declare
            use type Aquarius.Tokens.Token;
            Text : constant String := Slice (Entity.Edit_Buffer, Start, Last);
         begin

            if Entity.Update_Tree /= null then
               if Tok = Entity.Update_Tree.Get_Token then
                  --  replace the text of the existing token
                  Ada.Text_IO.Put_Line
                    ("Replace: ["
                     & Entity.Update_Tree.Text
                     & "] -> ["
                     & Text
                     & "]");
                  Entity.Update_Tree.Fill (Text);
                  Entity.Update_Tree := null;
                  --  TODO: update semantics
               else
                  --  we edited a token and changed it to something else
                  --  right now we're fucked
                  null;
               end if;
            else
               --  creating a new token

--                 Ada.Text_IO.Put_Line
--                   ("Parsing into: "
--                    & Aquarius.Trees.Cursors.Image
--                      (Aquarius.Programs.Parser.Get_Cursor
--                         (Entity.Parse_Context)));

               if Token_OK
                 (Tok, Aquarius.Source.No_Source_Position,
                  Entity.Parse_Context)
               then
                  Parse_Token
                    (Tok, Aquarius.Source.No_Source_Position,
                     Text, Entity.Parse_Context);
                  Ada.Text_IO.Put_Line
                    ("After parse: "
                     & Aquarius.Trees.Cursors.Image
                       (Aquarius.Programs.Parser.Get_Cursor
                            (Entity.Parse_Context)));
                  Entity.Cursors (Point).Tree :=
                    Aquarius.Programs.Program_Tree
                      (Aquarius.Trees.Cursors.Get_Left_Tree
                         (Aquarius.Programs.Parser.Get_Cursor
                            (Entity.Parse_Context)));
                  Entity.New_Line_Before_Buffer := False;
                  Got_Token := True;
               else
                  Ada.Text_IO.Put_Line ("parse failed");
                  Delete (Entity.Edit_Buffer,
                          Length (Entity.Edit_Buffer),
                          Length (Entity.Edit_Buffer));
                  Offset := Offset - 1;
                  Echo := False;
                  exit;
               end if;
            end if;

            --  remove the processed text from our buffer
            Ada.Strings.Unbounded.Delete (Entity.Edit_Buffer, 1, Last);
            Offset := Offset - Position_Offset (Last);
            Start := 1;
         end;
      end loop;

      if Got_Token and then
        not Aquarius.Programs.Parser.Is_Ambiguous
          (Entity.Parse_Context)
      then
         Komnenos.Entities.Visuals.Invalidate_Visuals (Entity.all);
         Echo := False;
      end if;

   end Check_Token;

   -----------------------------------
   -- Create_Aquarius_Source_Entity --
   -----------------------------------

   function Create_Aquarius_Source_Entity
     (Table            : not null access Entity_Table_Interface'Class;
      Name             : String;
      File_Name        : String;
      Class            : String;
      Line             : Natural;
      Column           : Natural;
      Top_Level        : Boolean;
      Compilation_Unit : Aquarius.Programs.Program_Tree;
      Entity_Spec      : Aquarius.Programs.Program_Tree;
      Entity_Body      : Aquarius.Programs.Program_Tree)
      return Entity_Reference
   is
      Key : constant String := Get_Key (File_Name, Line, Column, Name);
   begin
      if not Table.Exists (Key) then
         declare
            use type Aquarius.Programs.Program_Tree;
            Entity : Root_Aquarius_Source_Entity;
            Result : Entity_Reference;
         begin
            Entity.Create (Name, File_Name, Class, Line, Column);
            Entity.Top_Level := Top_Level;
            Entity.Compilation_Unit := Compilation_Unit;
            Entity.Grammar :=
              Aquarius.Trees.Properties.Get_Grammar (Compilation_Unit);
            Entity.Entity_Spec := Entity_Spec;
            Entity.Entity_Body := Entity_Body;
            Entity.Entity_Tree :=
              (if Entity_Body = null then Entity_Spec else Entity_Body);
            Aquarius.Programs.Parser.Initialise_Parse_Context
              (Context     => Entity.Parse_Context,
               Grammar     => Entity.Grammar,
               Root        => Entity.Entity_Tree,
               Interactive => True,
               Run_Actions => False);
            Aquarius.Programs.Parser.Set_Cursor
              (Entity.Parse_Context,
               Aquarius.Trees.Cursors.Left_Of_Tree (Entity.Entity_Tree));

            Result := new Root_Aquarius_Source_Entity'(Entity);
            Table.Add_Entity (Key, Result);
            return Result;
         end;
      else
         return Table.Get (Key);
      end if;
   end Create_Aquarius_Source_Entity;

   ---------------------
   -- Execute_Command --
   ---------------------

--     overriding procedure Execute_Command
--       (Item    : not null access Root_Aquarius_Source_Entity;
--        Command : Komnenos.Commands.Komnenos_Command)
--     is
--        use Komnenos.Commands;
--     begin
--        case Command.Command is
--           when No_Command =>
--              null;
--           when Move_Cursor_Command =>
--              case Command.Units is
--                 when By_Character =>
--                    if Command.Offset > 0 then
--                       for I in 1 .. Command.Offset loop
--                          Item.Forward_Character;
--                       end loop;
--                    elsif Command.Offset < 0 then
--                       for I in 1 .. -Command.Offset loop
--                          Item.Backward_Character;
--                       end loop;
--                    else
--                       null;
--                    end if;
--                 when By_Token =>
--                    null;
--                 when By_Line =>
--                    null;
--                 when By_Fragment =>
--                    null;
--              end case;
--
--           when Set_Cursor_Command =>
--              Set_Cursor (Item, Command.New_Position);
--
--           when Insert_Character_Command =>
--              Item.Insert_Character (Command.Ch);
--
--           when New_Line_Command =>
--              Item.Insert_New_Line;
--
--        end case;
--     end Execute_Command;

   ----------------------------
   -- Find_Entity_Containing --
   ----------------------------

--     function Find_Entity_Containing
--       (Table     : not null access Entity_Table_Interface'Class;
--        Location  : File_Location)
--        return Entity_Reference
--     is
--     begin
--        return Table.Get_Reference (Location);
--     end Find_Entity_Containing;

   -----------------------
   -- Forward_Character --
   -----------------------

   procedure Forward_Character
     (Item   : in out Root_Aquarius_Source_Entity'Class;
      Cursor : Cursor_Type)
   is
      use Ada.Strings.Unbounded;
      use Aquarius.Layout;
      Tree         : Aquarius.Programs.Program_Tree renames
                       Item.Cursors (Cursor).Tree;
      Offset       : Position_Offset renames
                       Item.Cursors (Cursor).Offset;
      New_Position : Position :=
                       Tree.Layout_Start_Position
                         + Offset + 1;
      Leaving      : Boolean := False;
      Token_Length : constant Natural :=
                       (if Cursor = Point
                        then Length (Item.Edit_Buffer)
                        else Tree.Text'Length);
   begin
      Offset := Offset + 1;

      if Offset > Position (Token_Length) then
         Leaving := True;
      elsif Offset = Position (Token_Length)
        and then Tree.Is_Reserved_Terminal
      then
         Leaving := True;
      end if;

      if Leaving then
         if Cursor = Point and then Item.Buffer_Changed then
            Item.Finish_Edit;
         end if;

         declare
            use Aquarius.Programs;
            Next_Terminal : constant Program_Tree :=
                              Tree.Scan_Terminal (1);
         begin

            if Next_Terminal = null or else not Next_Terminal.Is_Terminal then
               Offset := Position_Offset (Token_Length);
               New_Position := Tree.Layout_End_Position;
            elsif Next_Terminal.Layout_Line > Tree.Layout_Line then
               New_Position := Next_Terminal.Layout_Start_Position;
            end if;
         end;
      end if;

      Item.Set_Cursor (Cursor, New_Position);
      Komnenos.Entities.Visuals.Update_Cursor (Item, Cursor, New_Position);

   end Forward_Character;

   ----------------
   -- Get_Column --
   ----------------

   overriding function Get_Column
     (Entity   : Root_Aquarius_Source_Entity;
      Position : Aquarius.Layout.Position)
      return Aquarius.Layout.Column_Number
   is
      use Aquarius.Layout;
      use type Aquarius.Programs.Program_Tree;
      Terminal : constant Aquarius.Programs.Program_Tree :=
                   Entity.Entity_Tree.Find_Local_Node_At (Position);
   begin
      if Terminal = null then
         return 1;
      else
         return Terminal.Layout_Start_Column
           + Column_Offset (Position - Terminal.Layout_Start_Position);
      end if;
   end Get_Column;

   ----------------
   -- Get_Cursor --
   ----------------

   overriding function Get_Cursor
     (Entity : Root_Aquarius_Source_Entity;
      Cursor : Cursor_Type)
      return Aquarius.Layout.Position
   is
      use Aquarius.Layout;
      Tree   : Aquarius.Programs.Program_Tree renames
                 Entity.Cursors (Cursor).Tree;
      Offset : Position_Offset renames
                 Entity.Cursors (Cursor).Offset;
   begin
      return Tree.Layout_Start_Position + Offset;
   end Get_Cursor;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (File_Name : String;
                     Line      : Natural;
                     Column    : Natural;
                     Name      : String)
                     return String
   is
   begin
      return File_Name & Integer'Image (-Line)
        & Integer'Image (-Column)
        & "-" & Name;
   end Get_Key;

   --------------
   -- Get_Line --
   --------------

   overriding function Get_Line
     (Entity   : Root_Aquarius_Source_Entity;
      Position : Aquarius.Layout.Position)
      return Aquarius.Layout.Line_Number
   is
      use type Aquarius.Programs.Program_Tree;
      Terminal : constant Aquarius.Programs.Program_Tree :=
                   Entity.Entity_Tree.Find_Local_Node_At (Position);
   begin
      if Terminal = null then
         return 1;
      else
         return Terminal.Layout_Line;
      end if;
   end Get_Line;

   -----------------------
   -- Get_Start_Of_Line --
   -----------------------

   overriding function Get_Start_Of_Line
     (Entity : Root_Aquarius_Source_Entity;
      Line   : Aquarius.Layout.Line_Number)
      return Aquarius.Layout.Position
   is
      use Aquarius.Layout;
      use type Aquarius.Programs.Program_Tree;
      Terminal : constant Aquarius.Programs.Program_Tree :=
                   Entity.Entity_Tree.Find_Local_First_Node_At_Line
                     (Line);
   begin
      if Terminal = null then
         return 1;
      else
         return Terminal.Layout_Start_Position
           - Position_Offset (Terminal.Layout_Start_Column) + 1;
      end if;
   end Get_Start_Of_Line;

   ----------------------
   -- Insert_Character --
   ----------------------

   procedure Insert_Character
     (Entity : in out Root_Aquarius_Source_Entity'Class;
      Ch     : Character)
   is
      use Aquarius.Layout;
      Tree           : Aquarius.Programs.Program_Tree renames
                         Entity.Cursors (Point).Tree;
      Offset         : Position_Offset renames
                         Entity.Cursors (Point).Offset;
      Buffer         : constant String :=
                         Ada.Strings.Unbounded.To_String (Entity.Edit_Buffer);
      New_Buffer     : constant String :=
                         Buffer (1 .. Natural (Offset))
                       & Ch
                         & Buffer (Natural (Offset) + 1 .. Buffer'Last);
      Complete       : Boolean;
      Have_Class     : Boolean;
      Unique         : Boolean;
      Class          : Aquarius.Tokens.Token_Class;
      Tok            : Aquarius.Tokens.Token;
      First          : Positive := 1;
      Last           : Natural;
   begin

      if Offset > 0 and then not Entity.Buffer_Changed then
         --  check to see if we can join this token to the active one
         Aquarius.Tokens.Scan
           (Frame      => Entity.Grammar.Frame,
            Text       => New_Buffer,
            Partial    => False,
            Complete   => Complete,
            Have_Class => Have_Class,
            Unique     => Unique,
            Class      => Class,
            Tok        => Tok,
            First      => First,
            Last       => Last,
            Token_OK   => null);

         if Last = New_Buffer'Last then
            --  we can update the current token
            Ada.Text_IO.Put_Line
              ("Editor: updating terminal "
               & Tree.Text);
            Entity.Update_Tree := Tree;
         else
            Entity.Update_Tree := null;
            if Last = Buffer'Last
              and then Natural (Offset) = Buffer'Last
            then
               --  starting a new token
               Entity.Edit_Buffer :=
                 Ada.Strings.Unbounded.Null_Unbounded_String;
               Offset := 0;
               Aquarius.Programs.Parser.Set_Cursor
                 (Entity.Parse_Context,
                  Aquarius.Trees.Cursors.Right_Of_Tree (Tree));
               Entity.Insert_Character (Ch);
               return;
            end if;
         end if;
      end if;

--        Ada.Text_IO.Put_Line
--          ("edit: [" & New_Buffer & "] at "
--           & Aquarius.Trees.Cursors.Image
--             (Aquarius.Programs.Parser.Get_Cursor
--                  (Item.Parse_Context)));

      Entity.Edit_Buffer :=
        Ada.Strings.Unbounded.To_Unbounded_String (New_Buffer);
      Offset := Offset + 1;

      declare
         Echo : Boolean;
      begin
         Entity.Check_Token (False, Echo);
         if Echo then
            Komnenos.Entities.Visuals.Insert_At_Cursor
              (Entity, Point, (1 => Ch));
         end if;
      end;

      Entity.Buffer_Changed := True;

      Komnenos.UI.Current_UI.Program_Store.On_Edit
        (Entity.Compilation_Unit);

   end Insert_Character;

   ---------------------
   -- Insert_New_Line --
   ---------------------

   procedure Insert_New_Line
     (Item : in out Root_Aquarius_Source_Entity'Class)
   is
      Echo : Boolean;
      pragma Unreferenced (Echo);
   begin

      Item.Insert_Character (' ');
      Item.New_Line_Before_Buffer := True;
      Komnenos.Entities.Visuals.Invalidate_Visuals (Item);

--        declare
--           Cursor : constant Aquarius.Trees.Cursors.Cursor :=
--                      Aquarius.Programs.Parser.Get_Cursor
--                        (Item.Parse_Context);
--           Program : Aquarius.Programs.Program_Tree;
--        begin
--           if not Aquarius.Trees.Cursors.Is_Off_Right (Cursor) then
--              Program :=
--                Aquarius.Programs.Program_Tree
--                  (Aquarius.Trees.Cursors.Get_Right_Tree (Cursor));
--              Program.Set_Vertical_Gap_Before (1);
--              Komnenos.Entities.Visuals.Invalidate_Visuals (Item);
--           end if;
--        end;
   end Insert_New_Line;

   -----------------
   -- Insert_Text --
   -----------------

   overriding procedure Insert_Text
     (Item     : in out Root_Aquarius_Source_Entity;
      Text     : String)
   is
   begin
      for Ch of Text loop
         if Character'Pos (Ch) = 10 then
            Item.Insert_New_Line;
         else
            Root_Aquarius_Source_Entity'Class (Item).Insert_Character (Ch);
         end if;
      end loop;
   end Insert_Text;

   --------------
   -- Log_Tree --
   --------------

   procedure Log_Tree (Tree : Aquarius.Programs.Program_Tree) is

      procedure Log (T : Aquarius.Programs.Program_Tree;
                     Start : Ada.Text_IO.Count);

      ---------
      -- Log --
      ---------

      procedure Log (T : Aquarius.Programs.Program_Tree;
                     Start : Ada.Text_IO.Count)
      is
         use Ada.Text_IO;
         New_Start : Count := Start;
      begin
         if T.Name /= "" then
            Set_Col (Start);
            Put_Line (T.Image);
            New_Start := Start + 2;
         end if;
         for I in 1 .. T.Child_Count loop
            Log (T.Program_Child (I), New_Start);
         end loop;
      end Log;

   begin
      Log (Tree, 1);
   end Log_Tree;

   -----------------
   -- Move_Cursor --
   -----------------

   overriding procedure Move_Cursor
     (Item     : in out Root_Aquarius_Source_Entity;
      Cursor   : Cursor_Type;
      Movement : Cursor_Movement_Type;
      Offset   : Aquarius.Layout.Position_Offset)
   is
      use Aquarius.Layout;
      Forward : constant Boolean := Offset >= 0;
      Count   : constant Aquarius.Layout.Count :=
                  Aquarius.Layout.Count (abs Offset);
   begin
      case Movement is
         when By_Character =>
            for I in 1 .. Count loop
               if Forward then
                  Item.Forward_Character (Cursor);
               else
                  Item.Backward_Character (Cursor);
               end if;
            end loop;
         when By_Word =>
            null;
         when By_Line =>
            null;
         when By_Page =>
            null;
      end case;
   end Move_Cursor;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Entity : not null access Root_Aquarius_Source_Entity;
      Visual : not null access Entity_Visual'Class)
   is
      use Aquarius.Layout;
      use type Aquarius.Programs.Program_Tree;
      Renderer : Aquarius.Rendering.Aquarius_Renderer :=
                   Komnenos.Fragments.Rendering.Fragment_Renderer
                     (Komnenos.Fragments.Fragment_Type (Visual),
                      Entity.Table);
      Program  : constant Aquarius.Programs.Program_Tree := Entity.Entity_Tree;
      Tree_Cursor : constant Aquarius.Trees.Cursors.Cursor :=
                      Aquarius.Programs.Parser.Get_Cursor
                        (Entity.Parse_Context);
      Edit_Line : Line_Number;
      Edit_Column : Column_Number;
      Partial  : constant String :=
                   Ada.Strings.Unbounded.To_String (Entity.Edit_Buffer);
      Edit_Tree : constant Aquarius.Programs.Program_Tree :=
                    Entity.Cursors (Point).Tree;
      Offset    : constant Position_Offset := Entity.Cursors (Point).Offset;

   begin
      Aquarius.Programs.Arrangements.Arrange
        (Item             => Program,
         Point            => Tree_Cursor,
         Partial_Length   => Partial'Length,
         New_Line_Partial => Entity.New_Line_Before_Buffer,
         Partial_Line     => Edit_Line,
         Partial_Column   => Edit_Column,
         Line_Length      => Visual.Width / 8);

      Renderer.Set_Theme (Aquarius.Themes.Active_Theme);

      Visual.Clear;

      Aquarius.Programs.Arrangements.Render
        (Program          => Program,
         Renderer         => Renderer,
         Point            => Tree_Cursor,
         Partial          => Partial,
         Partial_Line     => Edit_Line,
         Partial_Column   => Edit_Column);

      if Edit_Tree /= null then
         Visual.Set_Cursor
           (Point, Edit_Tree.Layout_Start_Position + Offset);
      end if;

   end Render;

   ----------------
   -- Scan_Token --
   ----------------

   procedure Scan_Token
     (Entity : not null access Root_Aquarius_Source_Entity'Class;
      Force  : in     Boolean;
      Length :    out Natural;
      Tok    :    out Aquarius.Tokens.Token)
   is
      use Ada.Strings.Unbounded;
      Complete    : Boolean;
      Have_Class  : Boolean;
      Unique      : Boolean;
      Class       : Aquarius.Tokens.Token_Class;
      First, Next : Natural := 1;
   begin
      Aquarius.Tokens.Scan
        (Frame      => Entity.Grammar.Frame,
         Text       => To_String (Entity.Edit_Buffer),
         Partial    => True,
         Complete   => Complete,
         Have_Class => Have_Class,
         Unique     => Unique,
         Class      => Class,
         Tok        => Tok,
         First      => First,
         Last       => Next,
         Token_OK   => null);

      if (Force and then Next > 0) or else
        (Have_Class and then Complete
         and then Next < Ada.Strings.Unbounded.Length (Entity.Edit_Buffer))
      then
         Length := Next;
      else
         Length := 0;
      end if;

   end Scan_Token;

   -------------------
   -- Select_Entity --
   -------------------

   overriding procedure Select_Entity
     (Entity : not null access Root_Aquarius_Source_Entity;
      Table  : access Entity_Table_Interface'Class;
      Parent : access Entity_Visual'Class;
      Visual : access Entity_Visual'Class;
      Offset : Natural)
   is
      use Ada.Strings.Unbounded;
      use type Aquarius.Programs.Program_Tree;
      Fragment : constant Komnenos.Fragments.Fragment_Type :=
                   (if Visual = null
                    then Komnenos.Fragments.Source.New_Source_Fragment
                      (Title => To_String (Entity.Description),
                       Path  => To_String (Entity.File_Name))
                    else Komnenos.Fragments.Fragment_Type (Visual));
   begin
      Entity.Table := Entity_Table_Access (Table);
      Fragment.Set_Entity_Key (Key (Entity.all));
      Fragment.Set_Content (Entity);
      Komnenos.Entities.Visuals.Bind_Visual (Fragment, Entity);

      Root_Aquarius_Source_Entity'Class (Entity.all).Render (Fragment);

      if Visual = null then
         Komnenos.UI.Current_UI.Place_Fragment
           (Parent, Offset, Fragment);
      end if;

      Fragment.Rendered;

   end Select_Entity;

   ----------------
   -- Set_Cursor --
   ----------------

   overriding procedure Set_Cursor
     (Entity   : in out Root_Aquarius_Source_Entity;
      Cursor   : Cursor_Type;
      Position : Aquarius.Layout.Position)
   is
      use Ada.Strings.Unbounded;
      use Aquarius.Layout;
      use Aquarius.Programs;
      Terminal : constant Program_Tree :=
                   Entity.Entity_Tree.Find_Local_Node_At
                     (Location => Position);
      Tree_Cursor : Aquarius.Trees.Cursors.Cursor;
      Edit_Tree   : Aquarius.Programs.Program_Tree renames
                      Entity.Cursors (Cursor).Tree;
      Offset      : Position_Offset renames
                      Entity.Cursors (Cursor).Offset;
   begin
      if Terminal /= null and then Terminal /= Edit_Tree then
         Ada.Text_IO.Put_Line
           ("Set_Cursor:"
            & Position'Img
            & " "
            & Terminal.Image);
         if Terminal.Layout_End_Position < Position then
            Entity.Edit_Buffer := Null_Unbounded_String;
            Tree_Cursor :=
              Aquarius.Trees.Cursors.Right_Of_Tree (Terminal);
            Edit_Tree := Terminal;
            if Terminal.Layout_End_Position = Position - 1 then
               Entity.Edit_Buffer := To_Unbounded_String (Terminal.Text);
               Offset := Position_Offset (Length (Entity.Edit_Buffer));
            else
               Offset := 0;
            end if;
         else
            Edit_Tree   := Terminal;
            Tree_Cursor :=
              Aquarius.Trees.Cursors.Left_Of_Tree (Terminal);
            Entity.Edit_Buffer := To_Unbounded_String (Terminal.Text);
            Offset := Position - Terminal.Layout_Start_Position;
            Entity.Update_Tree := Edit_Tree;
         end if;

         Aquarius.Programs.Parser.Set_Cursor
           (Entity.Parse_Context, Tree_Cursor);

      end if;

--           Komnenos.Entities.Visuals.Update_Cursor (Item, Position);

      declare
         Buffer : constant String := To_String (Entity.Edit_Buffer);
      begin
         Ada.Text_IO.Put_Line
           ("edit: ["
            & Buffer (1 .. Natural (Offset))
            & "|"
            & Buffer (Natural (Offset) + 1 .. Buffer'Last)
            & "]");
      end;

   end Set_Cursor;

   ---------------------
   -- Set_Entity_Body --
   ---------------------

   procedure Set_Entity_Body
     (Entity : Entity_Reference;
      Entity_Body : Aquarius.Programs.Program_Tree)
   is
   begin
      Root_Aquarius_Source_Entity (Entity.all).Entity_Body := Entity_Body;
   end Set_Entity_Body;

   -------------------
   -- Syntax_Entity --
   -------------------

   function Syntax_Entity
     (Table  : not null access Entity_Table_Interface'Class;
      Entity : Entity_Reference)
      return Entity_Reference
   is
      Program : constant Aquarius.Programs.Program_Tree :=
                  Root_Aquarius_Source_Entity (Entity.all).Compilation_Unit;
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                  Aquarius.Trees.Properties.Get_Grammar (Program);
      Syntax  : constant Aquarius.Programs.Program_Tree :=
                  Grammar.Get_EBNF_Tree;
   begin
      return Create_Aquarius_Source_Entity
        (Table            => Table,
         Name             => Grammar.Name,
         File_Name        => Syntax.Source_File_Name,
         Class            => "syntax",
         Line             => 1,
         Column           => 1,
         Top_Level        => False,
         Compilation_Unit => Syntax,
         Entity_Spec      => Syntax,
         Entity_Body      => null);
   end Syntax_Entity;

end Komnenos.Entities.Source.Aquarius_Source;
