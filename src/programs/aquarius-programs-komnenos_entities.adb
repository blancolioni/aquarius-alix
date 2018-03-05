with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Aqua.Execution;
with Aqua.Primitives;

with Aquarius.Grammars;
with Aquarius.Source;
with Aquarius.Tokens;
with Aquarius.Trees.Properties;

with Aquarius.Programs.Arrangements;
with Aquarius.Programs.Parser;

with Aquarius.Rendering.Komnenos_Renderer;
with Aquarius.Trees.Cursors;

with Komnenos.Entities.Tables;
with Komnenos.Entities.Visual_Manager;
with Komnenos.Fragments;
with Komnenos.Themes;
with Komnenos.UI;

package body Aquarius.Programs.Komnenos_Entities is

   function Get_Key (File_Name : String;
                     Line      : Komnenos.Line_Number;
                     Column    : Komnenos.Column_Number;
                     Name      : String)
                     return String;

   type Root_Aquarius_Source_Entity is
     new Komnenos.Entities.Root_Entity_Reference with
      record
         Top_Level              : Boolean;
         Changed                : Boolean;
         Compilation_Unit       : Aquarius.Programs.Program_Tree;
         Grammar                : Aquarius.Grammars.Aquarius_Grammar;
         Defining_Name          : Aquarius.Programs.Program_Tree;
         Entity_Spec            : Aquarius.Programs.Program_Tree;
         Entity_Body            : Aquarius.Programs.Program_Tree;
         Entity_Tree            : Aquarius.Programs.Program_Tree;
         Update_Tree            : Aquarius.Programs.Program_Tree;
         Cursors                : Komnenos.Cursor_Position_Array :=
                                    (others => 0);
         Edit_Tree              : Aquarius.Programs.Program_Tree;
         Left_Of_Tree           : Boolean := True;
         Buffer_Offset          : Komnenos.Text_Offset := 0;
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
      Table  : access Komnenos.Entities.Entity_Table_Interface'Class;
      Parent : access Komnenos.Entities.Entity_Visual'Class;
      Visual : access Komnenos.Entities.Entity_Visual'Class;
      Offset : Komnenos.Pixel_Offset);

   overriding procedure Render
     (Entity : not null access Root_Aquarius_Source_Entity;
      Visual : not null access Komnenos.Entities.Entity_Visual'Class);

   overriding function Get_Cursor
     (Entity : Root_Aquarius_Source_Entity;
      Cursor : Komnenos.Cursor_Type)
      return Komnenos.Text_Position;

   overriding procedure Set_Cursor
     (Entity   : in out Root_Aquarius_Source_Entity;
      Cursor   : Komnenos.Cursor_Type;
      Position : Komnenos.Text_Position);

   overriding function Get_Line
     (Entity   : Root_Aquarius_Source_Entity;
      Position : Komnenos.Text_Position)
      return Komnenos.Line_Number;

   overriding function Get_Column
     (Entity   : Root_Aquarius_Source_Entity;
      Position : Komnenos.Text_Position)
      return Komnenos.Column_Number;

   overriding function Get_Start_Of_Line
     (Entity : Root_Aquarius_Source_Entity;
      Line   : Komnenos.Line_Number)
      return Komnenos.Text_Position;

   overriding procedure Move_Cursor
     (Item     : in out Root_Aquarius_Source_Entity;
      Cursor   : Komnenos.Cursor_Type;
      Movement : Komnenos.Text_Movement);

   overriding procedure Insert_Text
     (Item     : in out Root_Aquarius_Source_Entity;
      Text     : String);

   overriding procedure Delete_Region
     (Entity     : in out Root_Aquarius_Source_Entity);

   overriding function Get_Region_Text
     (Entity : Root_Aquarius_Source_Entity;
      End_1  : Komnenos.Cursor_Type;
      End_2  : Komnenos.Cursor_Type)
      return String
   is ("");

   procedure Forward_Character
     (Item   : in out Root_Aquarius_Source_Entity'Class);

   procedure Backward_Character
     (Entity   : in out Root_Aquarius_Source_Entity'Class);

   procedure Insert_Character
     (Entity : in out Root_Aquarius_Source_Entity'Class;
      Ch     : Character);

   procedure Insert_New_Line
     (Entity : in out Root_Aquarius_Source_Entity'Class);

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

   procedure Log_Tree (Tree : Aquarius.Programs.Program_Tree)
     with Unreferenced;

   function Show_Buffer
     (Entity : Root_Aquarius_Source_Entity'Class)
      return String;

   function Handle_Create_Entity
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   function Handle_Cross_Reference
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   function Handle_Get_Entity
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   ------------------
   -- Add_Handlers --
   ------------------

   procedure Add_Handlers is
   begin
      Aqua.Primitives.New_Primitive_Function
        (Name           => "tree__create_entity",
         Argument_Count => 6,
         Handler        => Handle_Create_Entity'Access);
      Aqua.Primitives.New_Primitive_Function
        (Name           => "tree__cross_reference",
         Argument_Count => 5,
         Handler        => Handle_Cross_Reference'Access);
      Aqua.Primitives.New_Primitive_Function
        (Name           => "tree__find_entity",
         Argument_Count => 4,
         Handler        => Handle_Get_Entity'Access);
   end Add_Handlers;

   ------------------------
   -- Backward_Character --
   ------------------------

   procedure Backward_Character
     (Entity : in out Root_Aquarius_Source_Entity'Class)
   is
      use Ada.Strings.Unbounded;
      use Komnenos;
      New_Position : Text_Position renames
                       Entity.Cursors (Komnenos.Point);
      Offset       : Text_Offset renames Entity.Buffer_Offset;
   begin

      if Offset > 0 then
         Offset := Offset - 1;
      else
         if Entity.Buffer_Changed then
            Entity.Finish_Edit;
         end if;

         declare
            use Aquarius.Programs;
            Next_Terminal : constant Program_Tree :=
                              Entity.Edit_Tree.Scan_Terminal (-1);
         begin
            if Next_Terminal = null or else not Next_Terminal.Is_Terminal then
               Offset := 0;
            else
               Entity.Edit_Tree := Next_Terminal;
               Entity.Left_Of_Tree := True;
               Offset := New_Position
                 - Text_Position (Next_Terminal.Layout_Start_Position);
            end if;
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
      use Komnenos;

      Tok       : Aquarius.Tokens.Token;
      Last      : Natural;
      Got_Token : Boolean := False;
      Start     : Natural :=
                    Index_Non_Blank (Entity.Edit_Buffer);
      Offset    : Komnenos.Text_Offset renames
                    Entity.Buffer_Offset;
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
--                    Ada.Text_IO.Put_Line
--                      ("After parse: "
--                       & Aquarius.Trees.Cursors.Image
--                         (Aquarius.Programs.Parser.Get_Cursor
--                              (Entity.Parse_Context)));
                  Entity.Edit_Tree :=
                    Aquarius.Programs.Program_Tree
                      (Aquarius.Trees.Cursors.Get_Left_Tree
                         (Aquarius.Programs.Parser.Get_Cursor
                            (Entity.Parse_Context)));
                  Entity.Left_Of_Tree := False;
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
            Offset := Offset - Text_Offset (Last);
            Start := 1;
         end;
      end loop;

      if Got_Token and then
        not Aquarius.Programs.Parser.Is_Ambiguous
          (Entity.Parse_Context)
      then
         Komnenos.Entities.Visual_Manager.Invalidate_Visuals (Entity.all);
         Echo := False;
         Ada.Text_IO.Put_Line ("after check_token: " & Entity.Show_Buffer);

      end if;

   end Check_Token;

   -----------------------------------
   -- Create_Aquarius_Source_Entity --
   -----------------------------------

   function Create_Aquarius_Source_Entity
     (Table            : not null access
        Komnenos.Entities.Entity_Table_Interface'Class;
      Name             : String;
      Qualified_Name   : String;
      Class_Name       : String;
      Top_Level        : Boolean;
      Compilation_Unit : not null access Program_Tree_Type'Class;
      Defining_Name    : not null access Program_Tree_Type'Class;
      Entity_Spec      : not null access Program_Tree_Type'Class;
      Entity_Body      : access Program_Tree_Type'Class)
      return Komnenos.Entities.Entity_Reference
   is
      File_Name : constant String :=
                    Aquarius.Names.To_String
                      (Compilation_Unit.Source_File_Name);
      Key : constant String :=
              Get_Key
                (File_Name => File_Name,
                 Line      => Defining_Name.Source_Line,
                 Column    => Defining_Name.Source_Column,
                 Name      => Name);
   begin
      if not Table.Exists (Key) then
         declare
            use type Aquarius.Programs.Program_Tree;
            Entity : Root_Aquarius_Source_Entity;
            Result : Komnenos.Entities.Entity_Reference;
         begin
            Entity.Create
              (Key              => Key,
               Identifier       => Name,
               Full_Name        => Qualified_Name,
               Class_Name       => Class_Name,
               Path             => File_Name,
               Display_Text     => Name,
               Description      => File_Name);

            Entity.Top_Level := Top_Level;
            Entity.Defining_Name := Program_Tree (Defining_Name);
            Entity.Compilation_Unit := Program_Tree (Compilation_Unit);
            Entity.Grammar :=
              Aquarius.Trees.Properties.Get_Grammar (Compilation_Unit);
            Entity.Entity_Spec := Program_Tree (Entity_Spec);
            Entity.Entity_Body := Program_Tree (Entity_Body);
            Entity.Entity_Tree :=
              (if Entity_Body = null
               then Entity.Entity_Spec
               else Entity.Entity_Body);
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

   -----------------------------------
   -- Create_Aquarius_Source_Entity --
   -----------------------------------

   procedure Create_Aquarius_Source_Entity
     (Table            : not null access
        Komnenos.Entities.Entity_Table_Interface'Class;
      Name             : String;
      Qualified_Name   : String;
      Class_Name       : String;
      Top_Level        : Boolean;
      Compilation_Unit : not null access Program_Tree_Type'Class;
      Defining_Name    : not null access Program_Tree_Type'Class;
      Entity_Spec      : not null access Program_Tree_Type'Class;
      Entity_Body      : access Program_Tree_Type'Class)
   is
      Entity : constant Komnenos.Entities.Entity_Reference :=
                 Create_Aquarius_Source_Entity
                   (Table, Name, Qualified_Name, Class_Name,
                    Top_Level, Compilation_Unit, Defining_Name,
                    Entity_Spec, Entity_Body);
   begin
      pragma Unreferenced (Entity);
   end Create_Aquarius_Source_Entity;

   -------------------
   -- Delete_Region --
   -------------------

   overriding procedure Delete_Region
     (Entity     : in out Root_Aquarius_Source_Entity)
   is
      use Komnenos, Komnenos.Entities;
      Mark_Position : constant Text_Position :=
                        Entity.Get_Cursor (Mark);
      Point_Position : constant Text_Position :=
                         Entity.Get_Cursor (Point);
      Start          : constant Text_Position :=
                         Text_Position'Min (Mark_Position, Point_Position);
      Finish         : constant Text_Position :=
                         Text_Position'Max (Mark_Position, Point_Position);
      Length         : constant Text_Offset := Finish - Start;
      Remaining      : Text_Offset := Length;
      Backward       : constant Boolean := Mark_Position < Point_Position;
      Needs_Render   : Boolean := False;
   begin

      Ada.Text_IO.Put_Line
        ("delete: start ="
         & Start'Img
         & "; finish ="
         & Finish'Img
         & "; length ="
         & Length'Img
         & "; buffer = "
         & Entity.Show_Buffer);

      if Backward then

         while Remaining > 0
           and then Remaining >= Entity.Buffer_Offset
         loop
            declare
               use Aquarius.Programs;
               New_Edit_Tree : constant Program_Tree :=
                                 Entity.Edit_Tree.Scan_Terminal (-1);
               New_Offset    : constant Text_Offset :=
                                 Entity.Edit_Tree.Source_Position
                                   - New_Edit_Tree.Source_Position;
               Ancestor      : Aquarius.Trees.Tree;
               Left, Right   : Aquarius.Trees.Tree;
               Deleted_Tree  : Program_Tree;
            begin
               New_Edit_Tree.Common_Ancestor
                 (Right          => Entity.Edit_Tree,
                  Ancestor       => Ancestor,
                  Left_Ancestor  => Left,
                  Right_Ancestor => Right);
               Deleted_Tree := Program_Tree (Right);
               Ada.Text_IO.Put_Line ("Deleting: " & Deleted_Tree.Image);
               Ancestor.Remove_Child (Right);
               Remaining := Remaining - Entity.Buffer_Offset;

               declare
                  New_Buffer : constant String :=
                                 New_Edit_Tree.Text;
                  Num_Spaces : constant Natural :=
                                 Natural
                                   (Entity.Edit_Tree.Source_Position
                                    - New_Edit_Tree.Source_Position
                                    - 1);
                  Spaces     : constant String (1 .. Num_Spaces) :=
                                 (others => ' ');
               begin
                  Entity.Edit_Buffer :=
                    Ada.Strings.Unbounded.To_Unbounded_String
                      (New_Buffer & Spaces);
               end;

               Entity.Edit_Tree := New_Edit_Tree;
               Entity.Update_Tree := Entity.Edit_Tree;
               Entity.Buffer_Offset := New_Offset;

               Aquarius.Programs.Parser.Set_Cursor
                 (Entity.Parse_Context,
                  Aquarius.Trees.Cursors.Right_Of_Tree
                    (Entity.Edit_Tree));

               Ada.Text_IO.Put_Line
                 ("new buffer: " & Show_Buffer (Entity));

               Free (Deleted_Tree);

               Needs_Render := True;
            end;
         end loop;

         if Remaining > 0 then

            Ada.Strings.Unbounded.Delete
              (Entity.Edit_Buffer,
               Positive (Entity.Buffer_Offset - Remaining + 1),
               Natural (Entity.Buffer_Offset));
            Entity.Buffer_Offset := Entity.Buffer_Offset - Remaining;
            Entity.Update_Tree := Entity.Edit_Tree;

            Entity.Buffer_Changed := True;

            Ada.Text_IO.Put_Line
              ("after delete: "
               & Entity.Show_Buffer);
         end if;

         Entity.Cursors (Point) :=
           Entity.Cursors (Point) - Length;

         if Needs_Render then
            Komnenos.Entities.Visual_Manager.Invalidate_Visuals (Entity);
         else
            Komnenos.Entities.Visual_Manager.Delete_At_Cursor
              (Entity, Point, -Length);
         end if;

      end if;
   end Delete_Region;

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
     (Item   : in out Root_Aquarius_Source_Entity'Class)
   is
      use Ada.Strings.Unbounded;
      use Komnenos;
      Tree         : Aquarius.Programs.Program_Tree renames
                       Item.Edit_Tree;
      Offset       : Text_Offset renames Item.Buffer_Offset;
      New_Position : constant Text_Position :=
                       Item.Cursors (Komnenos.Point);
      Leaving      : Boolean := False;
      Token_Length : constant Natural :=
                       Length (Item.Edit_Buffer);
   begin
      Offset := Offset + 1;

      if Offset > Text_Offset (Token_Length) then
         Leaving := True;
      elsif Offset = Text_Offset (Token_Length)
        and then Tree.Is_Reserved_Terminal
      then
         Leaving := True;
      end if;

      if Leaving then
         if Item.Buffer_Changed then
            Item.Finish_Edit;
         end if;

         declare
            use Aquarius.Programs;
            Next_Terminal : constant Program_Tree :=
                              Tree.Scan_Terminal (1);
         begin
            if Next_Terminal.Source_Position >= New_Position then
               Item.Edit_Tree := Next_Terminal;
               Item.Left_Of_Tree := True;
               Item.Buffer_Offset :=
                 New_Position - Next_Terminal.Source_Position;
            end if;
         end;
      end if;

   end Forward_Character;

   ----------------
   -- Get_Column --
   ----------------

   overriding function Get_Column
     (Entity   : Root_Aquarius_Source_Entity;
      Position : Komnenos.Text_Position)
      return Komnenos.Column_Number
   is
      use Komnenos;
      use type Aquarius.Programs.Program_Tree;
      Terminal : constant Aquarius.Programs.Program_Tree :=
                   Entity.Entity_Tree.Find_Local_Node_At
                     (Aquarius.Layout.Position (Position));
   begin
      if Terminal = null then
         return 1;
      else
         return Terminal.Source_Column
           + Column_Offset (Position - Terminal.Source_Position);
      end if;
   end Get_Column;

   ----------------
   -- Get_Cursor --
   ----------------

   overriding function Get_Cursor
     (Entity : Root_Aquarius_Source_Entity;
      Cursor : Komnenos.Cursor_Type)
      return Komnenos.Text_Position
   is
   begin
      return Entity.Cursors (Cursor);
   end Get_Cursor;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (File_Name : String;
                     Line      : Komnenos.Line_Number;
                     Column    : Komnenos.Column_Number;
                     Name      : String)
                     return String
   is
      use Komnenos;
   begin
      return File_Name & Line_Offset'Image (-Line)
        & Column_Offset'Image (-Column)
        & "-" & Name;
   end Get_Key;

   --------------
   -- Get_Line --
   --------------

   overriding function Get_Line
     (Entity   : Root_Aquarius_Source_Entity;
      Position : Komnenos.Text_Position)
      return Komnenos.Line_Number
   is
      use type Aquarius.Programs.Program_Tree;
      Terminal : constant Aquarius.Programs.Program_Tree :=
                   Entity.Entity_Tree.Find_Local_Node_At
                     (Aquarius.Layout.Position (Position));
   begin
      if Terminal = null then
         return 1;
      else
         return Terminal.Source_Line;
      end if;
   end Get_Line;

   -----------------------
   -- Get_Start_Of_Line --
   -----------------------

   overriding function Get_Start_Of_Line
     (Entity : Root_Aquarius_Source_Entity;
      Line   : Komnenos.Line_Number)
      return Komnenos.Text_Position
   is
      use Komnenos;
      use type Aquarius.Programs.Program_Tree;
      Terminal : constant Aquarius.Programs.Program_Tree :=
                   Entity.Entity_Tree.Find_Local_First_Node_At_Line
                     (Aquarius.Layout.Line_Number (Line));
   begin
      if Terminal = null then
         return 1;
      else
         return Terminal.Source_Position
           - Text_Offset (Terminal.Source_Column) + 1;
      end if;
   end Get_Start_Of_Line;

   --------------------------
   -- Handle_Create_Entity --
   --------------------------

   function Handle_Create_Entity
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      use type Aqua.Word;
      Spec           : constant Program_Tree :=
                         To_Program_Tree (Context, Arguments (1));
      Defining_Name  : constant Program_Tree :=
                         To_Program_Tree (Context, Arguments (2));
      Name           : constant String := Context.To_String (Arguments (3));
      Qualified_Name : constant String := Context.To_String (Arguments (4));
      Class_Name     : constant String := Context.To_String (Arguments (5));
      Top_Level      : constant Boolean :=
                         Arguments (6) /= 0;
      Entity  : constant Komnenos.Entities.Entity_Reference :=
                  Create_Aquarius_Source_Entity
                    (Table            => Komnenos.Entities.Tables.Table ("/"),
                     Name             => Name,
                     Qualified_Name   => Qualified_Name,
                     Class_Name       => Class_Name,
                     Top_Level        => Top_Level,
                     Compilation_Unit => Spec.Program_Root,
                     Defining_Name    => Defining_Name,
                     Entity_Spec      => Spec,
                     Entity_Body      => null);
   begin
      return Context.Return_Class_Instance ("komnenos__entity", Entity);
   end Handle_Create_Entity;

   ----------------------------
   -- Handle_Cross_Reference --
   ----------------------------

   function Handle_Cross_Reference
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      use Komnenos.Entities;
      Referrer_Tree     : constant Program_Tree :=
                            To_Program_Tree (Context, Arguments (1));
      Table_Name        : constant String :=
                            Context.To_String (Arguments (2));
      Referrer_Entity   : constant Entity_Reference :=
                            To_Entity (Context, Arguments (3));
      Referenced_Entity : constant Entity_Reference :=
                            To_Entity (Context, Arguments (4));
      Ref_Type          : constant String :=
                            Context.To_String (Arguments (5));
   begin

      Ada.Text_IO.Put_Line
        (Ref_Type & " [" & Table_Name & "] "
         & Referrer_Tree.Show_Location & ":"
         & Referrer_Tree.Text
         & " -> "
         & Referenced_Entity.Class
         & " " & Referenced_Entity.Name);

      Komnenos.Entities.Tables.Table (Table_Name).Add_Cross_Reference
        (Item      => Referenced_Entity,
         Referrer  => Referrer_Entity,
         File_Name =>
           Aquarius.Names.To_String (Referrer_Tree.Source_File_Name),
         Line      => Referrer_Tree.Source_Line,
         Column    => Referrer_Tree.Source_Column,
         Ref_Type  => Ref_Type);
      return 1;
   end Handle_Cross_Reference;

   -----------------------
   -- Handle_Get_Entity --
   -----------------------

   function Handle_Get_Entity
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      use Komnenos.Entities;
      Table_Name        : constant String :=
                            Context.To_String (Arguments (2));
      Entity_Name       : constant String :=
                            Context.To_String (Arguments (3));
      Class_Name        : constant String :=
                            Context.To_String (Arguments (4));
      Entity            : constant Entity_Reference :=
                            Komnenos.Entities.Tables.Table (Table_Name)
                            .Find (Entity_Name, Class_Name);
   begin
      if Entity = null then
         return 0;
      else
         return Context.Return_Class_Instance
           ("komnenos__entity", Entity);
      end if;
   end Handle_Get_Entity;

   ----------------------
   -- Insert_Character --
   ----------------------

   procedure Insert_Character
     (Entity : in out Root_Aquarius_Source_Entity'Class;
      Ch     : Character)
   is
      use Komnenos;
      Tree           : Aquarius.Programs.Program_Tree renames
                         Entity.Edit_Tree;
      Offset         : Text_Offset renames Entity.Buffer_Offset;
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
            Komnenos.Entities.Visual_Manager.Insert_At_Cursor
              (Entity, Komnenos.Point, (1 => Ch));

            if Ch = Character'Val (10) then
               declare
                  use Aquarius.Programs;
                  use Ada.Strings.Unbounded;
                  Edit_Tree     : constant Aquarius.Programs.Program_Tree :=
                                    Entity.Edit_Tree;
                  Start_Line    : constant Program_Tree :=
                                    Edit_Tree.Start_Of_Line;
                  Next_Line     : constant Program_Tree :=
                                    Edit_Tree.Scan_Terminal (1);
                  Before_Indent : constant Column_Number :=
                                    Start_Line.Source_Column;
                  After_Indent  : constant Column_Number :=
                                    Next_Line.Source_Column;
                  New_Indent    : constant Column_Number :=
                                    Column_Number'Max
                                      (Before_Indent, After_Indent);
                  Spaces                           : constant String
                    (1 .. Positive (New_Indent) - 1) :=
                                                       (others => ' ');
               begin
                  Ada.Text_IO.Put_Line
                    ("NL: before indent =" & Before_Indent'Img
                     & "; after indent =" & After_Indent'Img);

                  Entity.Buffer_Offset := Text_Offset (New_Indent);
                  Entity.Edit_Buffer :=
                    Entity.Edit_Buffer & Spaces;
                  Komnenos.Entities.Visual_Manager.Insert_At_Cursor
                    (Entity, Point, Spaces);
               end;
            end if;
         end if;
      end;

      Entity.Buffer_Changed := True;

      Komnenos.Entities.Tables.Table ("/").Program_Store.On_Edit
        (Entity.Compilation_Unit);

   end Insert_Character;

   ---------------------
   -- Insert_New_Line --
   ---------------------

   procedure Insert_New_Line
     (Entity : in out Root_Aquarius_Source_Entity'Class)
   is
      Echo : Boolean;
      pragma Unreferenced (Echo);
   begin

--        if Entity.Buffer_Changed then
--           --  FIXME
--           Entity.Insert_Character (' ');
--        end if;

      Entity.Update_Tree := null;
      Entity.Buffer_Changed := True;
      Entity.Edit_Buffer := Ada.Strings.Unbounded.Null_Unbounded_String;
      Entity.Buffer_Offset := 0;
      Entity.Left_Of_Tree := False;

      Entity.New_Line_Before_Buffer := True;

      declare
         use Aquarius.Layout;
         Edit_Tree : constant Aquarius.Programs.Program_Tree :=
                       Entity.Edit_Tree;
         Before_Indent : constant Column_Number :=
                           Edit_Tree.Start_Of_Line.Layout_Start_Column;
         After_Indent  : constant Column_Number :=
                           Edit_Tree.Scan_Terminal (1).Layout_Start_Column;
         New_Indent    : constant Column_Number :=
                           Column_Number'Max (Before_Indent, After_Indent);
         Spaces        : constant String (1 .. Positive (New_Indent) - 1) :=
                           (others => ' ');
      begin
         Ada.Text_IO.Put_Line
           ("NL: before indent =" & Before_Indent'Img
            & "; after indent =" & After_Indent'Img);

         Entity.Buffer_Offset := Komnenos.Text_Offset (New_Indent);
         Entity.Edit_Buffer :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (' ' & Spaces);
         Komnenos.Entities.Visual_Manager.Insert_At_Cursor
           (Entity, Komnenos.Point, Character'Val (10) & Spaces);
      end;

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
--              Komnenos.Entities.Visual_Manager.Invalidate_Visuals (Item);
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
      Cursor   : Komnenos.Cursor_Type;
      Movement : Komnenos.Text_Movement)
   is
      use Komnenos;
      Forward : constant Boolean := Movement.Offset >= 0;
      Count   : constant Aquarius.Layout.Count :=
                  Aquarius.Layout.Count (abs Movement.Offset);
      Current : Text_Position renames Item.Cursors (Cursor);
   begin
      case Movement.Unit is
         when Character_Unit =>
            for I in 1 .. Count loop
               if Forward then
                  Current := Current + 1;
                  if Cursor = Point then
                     Item.Forward_Character;
                  end if;
               else
                  if Current > 0 then
                     Current := Current - 1;
                     if Cursor = Point then
                        Item.Backward_Character;
                     end if;
                  end if;
               end if;
            end loop;
         when Word_Unit =>
            null;
         when Line_Unit =>
            null;
         when Page_Unit =>
            null;
         when Buffer_Unit =>
            null;
      end case;
   end Move_Cursor;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Entity : not null access Root_Aquarius_Source_Entity;
      Visual : not null access Komnenos.Entities.Entity_Visual'Class)
   is
      use Komnenos;
      use type Aquarius.Programs.Program_Tree;
      Renderer : Aquarius.Rendering.Aquarius_Renderer :=
                   Aquarius.Rendering.Komnenos_Renderer.Fragment_Renderer
                     (Komnenos.Fragments.Text_Fragment (Visual),
                      Komnenos.Entities.Tables.Table ("/"));
      Program  : constant Aquarius.Programs.Program_Tree := Entity.Entity_Tree;
      Tree_Cursor : constant Aquarius.Trees.Cursors.Cursor :=
                      Aquarius.Programs.Parser.Get_Cursor
                        (Entity.Parse_Context);
      Edit_Line   : Aquarius.Layout.Line_Number;
      Edit_Column : Aquarius.Layout.Column_Number;
      Partial  : constant String :=
                   Ada.Strings.Unbounded.To_String (Entity.Edit_Buffer);
      Edit_Tree : constant Aquarius.Programs.Program_Tree :=
                    Entity.Edit_Tree;
      Offset    : constant Text_Offset := Entity.Buffer_Offset;
      Left_Of_Tree : constant Boolean := Entity.Left_Of_Tree;
   begin
      Aquarius.Programs.Arrangements.Arrange
        (Item             => Program,
         Point            => Tree_Cursor,
         Partial          => Partial,
         Updating         => Entity.Update_Tree /= null,
         Partial_Line     => Edit_Line,
         Partial_Column   => Edit_Column,
         Line_Length      => Natural (Visual.Width) / 8);

      Renderer.Set_Theme (Komnenos.Themes.Active_Theme);

      Visual.Clear;

      Aquarius.Programs.Arrangements.Render
        (Program          => Program,
         Renderer         => Renderer,
         Point            => Tree_Cursor,
         Partial          => Partial,
         Updating         => Entity.Update_Tree /= null,
         Partial_Line     => Edit_Line,
         Partial_Column   => Edit_Column);

      if Edit_Tree /= null then
         Ada.Text_IO.Put_Line
           ("Set cursor: edit-tree = " & Edit_Tree.Text
            & Aquarius.Layout.Position_Offset'Image
              (Edit_Tree.Layout_Start_Position)
            & " .."
            & Aquarius.Layout.Position_Offset'Image
              (Edit_Tree.Layout_End_Position)
            & "; side = "
            & (if Left_Of_Tree then "left" else "right")
            & "; offset ="
            & Offset'Img);

         declare
            use Aquarius.Layout;
            New_Offset   : constant Aquarius.Layout.Position_Offset :=
                             Aquarius.Layout.Position_Offset (Offset);
            New_Position : constant Aquarius.Layout.Position :=
                             (if Left_Of_Tree
                              then Edit_Tree.Layout_Start_Position + New_Offset
                              else Edit_Tree.Layout_End_Position
                              + 1 + New_Offset);
         begin
            Komnenos.Entities.Text_Entity_Visual'Class (Visual.all)
              .Set_Cursor (Point, Text_Position (New_Position));
         end;
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
      Buffer      : String :=
                      To_String (Entity.Edit_Buffer);
   begin
      for I in Buffer'Range loop
         if Buffer (I) = Character'Val (10) then
            Buffer (I) := ' ';
         end if;
      end loop;

      Aquarius.Tokens.Scan
        (Frame      => Entity.Grammar.Frame,
         Text       => Buffer,
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
         and then Next <= Ada.Strings.Unbounded.Length (Entity.Edit_Buffer))
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
      Table  : access Komnenos.Entities.Entity_Table_Interface'Class;
      Parent : access Komnenos.Entities.Entity_Visual'Class;
      Visual : access Komnenos.Entities.Entity_Visual'Class;
      Offset : Komnenos.Pixel_Offset)
   is
      use Ada.Strings.Unbounded;
      use type Aquarius.Programs.Program_Tree;
      Fragment : constant Komnenos.Fragments.Fragment_Type :=
                   (if Visual = null
                    then Komnenos.Fragments.New_Text_Fragment (Entity)
                    else Komnenos.Fragments.Fragment_Type (Visual));
      pragma Unreferenced (Table);
   begin
--      Entity.Table := Entity_Table_Access (Table);
--        Fragment.Set_Entity_Key (Key (Entity.all));
--        Fragment.Set_Content (Entity);
      Komnenos.Entities.Visual_Manager.Bind_Visual (Fragment, Entity);

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
      Cursor   : Komnenos.Cursor_Type;
      Position : Komnenos.Text_Position)
   is
   begin
      Entity.Cursors (Cursor) := Position;

      case Cursor is
         when Komnenos.Point =>
            declare
               use Ada.Strings.Unbounded;
               use Aquarius.Layout;
               use Aquarius.Programs;
               use type Komnenos.Text_Position;
               Pos : constant Aquarius.Layout.Position :=
                       Aquarius.Layout.Position (Position);
               Terminal     : constant Program_Tree :=
                                Entity.Entity_Tree.Find_Local_Node_At (Pos);
               Tree_Cursor  : Aquarius.Trees.Cursors.Cursor;
            begin
               if Terminal /= null
                 and then Terminal /= Entity.Edit_Tree
               then
                  Entity.Left_Of_Tree := True;
                  if Terminal.Layout_End_Position < Pos then
                     Entity.Edit_Buffer := Null_Unbounded_String;
                     Tree_Cursor :=
                       Aquarius.Trees.Cursors.Right_Of_Tree (Terminal);
                     Entity.Edit_Tree := Terminal;

                     if Terminal.Layout_End_Position = Pos - 1 then
                        Entity.Edit_Buffer :=
                          To_Unbounded_String (Terminal.Text);
                        Entity.Buffer_Offset :=
                          Komnenos.Text_Offset (Length (Entity.Edit_Buffer));
                     else
                        Entity.Buffer_Offset := 0;
                     end if;
                  else
                     Entity.Edit_Tree := Terminal;
                     Tree_Cursor :=
                       Aquarius.Trees.Cursors.Left_Of_Tree (Terminal);
                     Entity.Edit_Buffer := To_Unbounded_String (Terminal.Text);
                     Entity.Update_Tree := Entity.Edit_Tree;
                     Entity.Buffer_Offset :=
                       Position - Terminal.Source_Position;
                  end if;

                  Aquarius.Programs.Parser.Set_Cursor
                    (Entity.Parse_Context, Tree_Cursor);

               end if;
            end;
         when others =>
            null;
      end case;
   end Set_Cursor;

   ---------------------
   -- Set_Entity_Body --
   ---------------------

   procedure Set_Entity_Body
     (Entity : Komnenos.Entities.Entity_Reference;
      Entity_Body : not null access Program_Tree_Type'Class)
   is
   begin
      Root_Aquarius_Source_Entity (Entity.all).Entity_Body :=
        Program_Tree (Entity_Body);
   end Set_Entity_Body;

   -----------------
   -- Show_Buffer --
   -----------------

   function Show_Buffer
     (Entity : Root_Aquarius_Source_Entity'Class)
      return String
   is
      use Ada.Strings.Unbounded;
      Buffer : constant String := To_String (Entity.Edit_Buffer);
      Offset : constant Natural :=
                 Natural (Entity.Buffer_Offset);
   begin
      return "["
        & Buffer (1 .. Offset)
        & "|"
        & Buffer (Offset + 1 .. Buffer'Last)
        & "]";
   end Show_Buffer;

   -------------------
   -- Syntax_Entity --
   -------------------

   function Syntax_Entity
     (Table  : not null access Komnenos.Entities.Entity_Table_Interface'Class;
      Entity : Komnenos.Entities.Entity_Reference)
      return Komnenos.Entities.Entity_Reference
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
         Qualified_Name   => Grammar.Name,
         Class_Name       => "syntax",
         Top_Level        => False,
         Compilation_Unit => Syntax,
         Defining_Name    => Syntax,
         Entity_Spec      => Syntax,
         Entity_Body      => null);
   end Syntax_Entity;

end Aquarius.Programs.Komnenos_Entities;
