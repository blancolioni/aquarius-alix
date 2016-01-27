with Ada.Text_IO;

with Aquarius.Grammars;
with Aquarius.Trees.Properties;

with Aquarius.Programs.Arrangements;
with Aquarius.Rendering;
with Aquarius.Themes;
with Aquarius.Trees.Cursors;

with Komnenos.Entities.Visuals;
with Komnenos.Fragments.Rendering;
with Komnenos.Fragments.Source;
with Komnenos.UI;

package body Komnenos.Entities.Source.Aquarius_Source is

   type Root_Aquarius_Source_Entity is
     new Root_Source_Entity_Reference with
      record
         Top_Level        : Boolean;
         Compilation_Unit : Aquarius.Programs.Program_Tree;
         Entity_Spec      : Aquarius.Programs.Program_Tree;
         Entity_Body      : Aquarius.Programs.Program_Tree;
         Entity_Tree      : Aquarius.Programs.Program_Tree;
         Tree_Cursor      : Aquarius.Trees.Cursors.Cursor;
         Edit_Tree        : Aquarius.Programs.Program_Tree;
         Edit_Buffer      : Ada.Strings.Unbounded.Unbounded_String;
         Buffer_Cursor    : Natural;
         Buffer_Changed   : Boolean := False;
         Invalidated      : Boolean := False;
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

   overriding procedure Execute_Command
     (Item    : not null access Root_Aquarius_Source_Entity;
      Command : Komnenos.Commands.Komnenos_Command);

   procedure Forward_Character
     (Item : not null access Root_Aquarius_Source_Entity'Class);

   procedure Backward_Character
     (Item : not null access Root_Aquarius_Source_Entity'Class);

   procedure Set_Cursor
     (Item     : not null access Root_Aquarius_Source_Entity'Class;
      Position : Aquarius.Layout.Position);

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
     (Item : not null access Root_Aquarius_Source_Entity'Class)
   is
      use Ada.Strings.Unbounded;
      use Aquarius.Layout;
   begin
      if Item.Buffer_Cursor > 0 then
         declare
            New_Position : Position := Item.Edit_Tree.Layout_Start_Position;
         begin
            Item.Buffer_Cursor := Item.Buffer_Cursor - 1;
            New_Position.Column :=
              New_Position.Column
                + Aquarius.Layout.Count (Item.Buffer_Cursor);
            Item.Set_Cursor (New_Position);
            Komnenos.Entities.Visuals.Update_Cursor (Item, New_Position);
         end;
      end if;
   end Backward_Character;

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
            Entity.Entity_Spec := Entity_Spec;
            Entity.Entity_Body := Entity_Body;
            Entity.Entity_Tree :=
              (if Entity_Body = null then Entity_Spec else Entity_Body);
            Entity.Tree_Cursor :=
              Aquarius.Trees.Cursors.Left_Of_Tree (Entity.Entity_Tree);
            Entity.Edit_Tree := null;
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

   overriding procedure Execute_Command
     (Item    : not null access Root_Aquarius_Source_Entity;
      Command : Komnenos.Commands.Komnenos_Command)
   is
      use Komnenos.Commands;
   begin
      case Command.Command is
         when No_Command =>
            null;
         when Move_Cursor_Command =>
            case Command.Units is
               when By_Character =>
                  if Command.Offset > 0 then
                     for I in 1 .. Command.Offset loop
                        Item.Forward_Character;
                     end loop;
                  elsif Command.Offset < 0 then
                     for I in 1 .. -Command.Offset loop
                        Item.Backward_Character;
                     end loop;
                  else
                     null;
                  end if;
               when By_Token =>
                  null;
               when By_Line =>
                  null;
               when By_Fragment =>
                  null;
            end case;

         when Set_Cursor_Command =>
            Set_Cursor (Item, Command.New_Position);
         when Insert_Character_Command =>
            Root_Entity_Reference (Item.all).Execute_Command (Command);
      end case;
   end Execute_Command;

   ----------------------------
   -- Find_Entity_Containing --
   ----------------------------

   function Find_Entity_Containing
     (Table     : not null access Entity_Table_Interface'Class;
      Location  : File_Location)
      return Entity_Reference
   is
   begin
      return Table.Get_Reference (Location);
   end Find_Entity_Containing;

   -----------------------
   -- Forward_Character --
   -----------------------

   procedure Forward_Character
     (Item : not null access Root_Aquarius_Source_Entity'Class)
   is
      use Ada.Strings.Unbounded;
      use Aquarius.Layout;
   begin
      if Item.Buffer_Cursor < Length (Item.Edit_Buffer) then
         declare
            New_Position : Position := Item.Edit_Tree.Layout_Start_Position;
         begin
            Item.Buffer_Cursor := Item.Buffer_Cursor + 1;
            New_Position.Column :=
              New_Position.Column
                + Aquarius.Layout.Count (Item.Buffer_Cursor);
            Item.Set_Cursor (New_Position);
            Komnenos.Entities.Visuals.Update_Cursor (Item, New_Position);
         end;
      end if;
   end Forward_Character;

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

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Entity : not null access Root_Aquarius_Source_Entity;
      Visual : not null access Entity_Visual'Class)
   is
      Renderer : constant Aquarius.Rendering.Aquarius_Renderer :=
                   Komnenos.Fragments.Rendering.New_Fragment_Renderer
                     (Komnenos.Fragments.Fragment_Type (Visual),
                      Entity.Table);
      Program  : constant Aquarius.Programs.Program_Tree := Entity.Entity_Tree;
   begin
      Aquarius.Programs.Arrangements.Arrange
        (Program,
         Line_Length => Visual.Width / 8);

      Renderer.Set_Theme (Aquarius.Themes.Active_Theme);

      Visual.Clear;

      Aquarius.Programs.Arrangements.Render
        (Program   => Program,
         Renderer  => Renderer,
         Point     => Entity.Tree_Cursor,
         Partial   =>
           Ada.Strings.Unbounded.To_String
             (Entity.Edit_Buffer));

      Visual.Invalidate;

   end Render;

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

   end Select_Entity;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor
     (Item     : not null access Root_Aquarius_Source_Entity'Class;
      Position : Aquarius.Layout.Position)
   is
      use Ada.Strings.Unbounded;
      use Aquarius.Layout;
      use Aquarius.Programs;
      Terminal : constant Program_Tree :=
                   Item.Entity_Tree.Find_Local_Node_At
                     (Location => Position);
   begin
      if Terminal /= null then
         if Terminal.Layout_End_Position < Position then
            Item.Edit_Buffer := Null_Unbounded_String;
            Item.Tree_Cursor :=
              Aquarius.Trees.Cursors.Right_Of_Tree (Terminal);
            Item.Edit_Tree := null;
            Item.Buffer_Cursor := 0;
         else
            Item.Edit_Tree   := Terminal;
            Item.Tree_Cursor :=
              Aquarius.Trees.Cursors.Left_Of_Tree (Terminal);
            Item.Edit_Buffer := To_Unbounded_String (Terminal.Text);
            Item.Buffer_Cursor :=
              Positive (Position.Column)
              - Positive (Terminal.Layout_Start_Position.Column);
         end if;

--           Komnenos.Entities.Visuals.Update_Cursor (Item, Position);

         declare
            Buffer : constant String := To_String (Item.Edit_Buffer);
         begin
            Ada.Text_IO.Put_Line
              ("edit: ["
               & Buffer (1 .. Item.Buffer_Cursor)
               & "|"
               & Buffer (Item.Buffer_Cursor + 1 .. Buffer'Last)
               & "]");
         end;
      end if;
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
