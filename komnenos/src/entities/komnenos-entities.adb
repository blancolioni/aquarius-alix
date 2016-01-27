with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Less_Case_Insensitive;
with Ada.Text_IO;

package body Komnenos.Entities is

   function Get_Key (File_Name : String;
                     Line      : Natural;
                     Column    : Natural)
                     return String;

   function Display_Text_Less_Than
     (Left, Right : Entity_Reference)
      return Boolean
   is (Ada.Strings.Unbounded.Less_Case_Insensitive
       (Left.Display_Text, Right.Display_Text));

   package Entity_Sorting is
     new Entity_Vectors.Generic_Sorting
       ("<" => Display_Text_Less_Than);

   -------------------------
   -- Add_Cross_Reference --
   -------------------------

   overriding procedure Add_Cross_Reference
     (Table        : in out Entity_Table;
      Item         : Entity_Reference;
      File_Name    : String;
      Line, Column : Natural;
      Ref_Type     : String)
   is
      Key : constant String := Get_Key (File_Name, Line, Column);
      Ref_Store : constant Ada.Strings.Unbounded.Unbounded_String :=
                    Ada.Strings.Unbounded.To_Unbounded_String (Ref_Type);
   begin
      if Table.X_Ref.Contains (Key) then
         Table.X_Ref (Key).Append
           ((Item, Ref_Store));
      else
         declare
            List : Cross_Reference_Lists.List;
         begin
            List.Append ((Item, Ref_Store));
            Table.X_Ref.Insert (Key, List);
         end;
      end if;

      if not Table.File_Map.Contains (File_Name) then
         Table.File_Vector.Append (File_Name);
         Table.File_Map.Insert (File_Name, Table.File_Vector.Last_Index);
      end if;

      Item.References.Append
        ((Table.File_Map (File_Name), Ref_Store, Line, Column));

   end Add_Cross_Reference;

   ----------------
   -- Add_Entity --
   ----------------

   overriding procedure Add_Entity
     (Table : in out Entity_Table;
      Key   : String;
      Item  : Entity_Reference)
   is
      Name : constant String := Item.Name;
   begin
      Item.Key := Ada.Strings.Unbounded.To_Unbounded_String (Key);
      Table.Table.Append (Item);
      Table.Map.Insert (Key, Item);
      if Table.Name_Map.Contains (Name) then
         Table.Name_Map (Name).Append (Item);
      else
         declare
            List : List_Of_Entities.List;
         begin
            List.Append (Item);
            Table.Name_Map.Insert (Name, List);
         end;
      end if;
   end Add_Entity;

   -----------
   -- Class --
   -----------

   function Class
     (Item : Root_Entity_Reference'Class)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Class);
   end Class;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item         : in out Root_Entity_Reference'Class;
      Identifier   : in String;
      Class_Name   : in String;
      Display_Text : in String := "";
      Description  : in String := "")
   is
   begin
      Item.Identifier :=
        Ada.Strings.Unbounded.To_Unbounded_String (Identifier);

      Item.Class :=
        Ada.Strings.Unbounded.To_Unbounded_String (Class_Name);

      if Display_Text /= "" then
         Item.Display_Text :=
           Ada.Strings.Unbounded.To_Unbounded_String (Display_Text);
      else
         Item.Display_Text := Item.Identifier;
      end if;

      if Description /= "" then
         Item.Description :=
           Ada.Strings.Unbounded.To_Unbounded_String (Description);
      else
         Item.Description := Item.Identifier;
      end if;

   end Create;

   ----------------------
   -- Cross_References --
   ----------------------

   overriding function Cross_References
     (Table        : Entity_Table;
      File_Name    : String;
      Line, Column : Positive;
      Enabled      : String := "")
      return Array_Of_Entities
   is
      Key : constant String := Get_Key (File_Name, Line, Column);
   begin
      if Table.X_Ref.Contains (Key) then
         declare
            List : constant Cross_Reference_Lists.List :=
                     Table.X_Ref (Key);
            Result : Array_Of_Entities (1 .. Natural (List.Length));
            Count  : Natural := 0;
         begin
            for Element of List loop
               if Enabled = "all" then
                  Count := Count + 1;
                  Result (Count) := Element.Entity;
               end if;
            end loop;
            return Result (1 .. Count);
         end;
      else
         declare
            Result : Array_Of_Entities (1 .. 0);
         begin
            return Result;
         end;
      end if;
   end Cross_References;

   -----------------
   -- Description --
   -----------------

   function Description
     (Item : Root_Entity_Reference)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Description);
   end Description;

   ------------------
   -- Display_Text --
   ------------------

   function Display_Text
     (Item : Root_Entity_Reference)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Display_Text);
   end Display_Text;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (Item : in out Root_Entity_Reference;
      Command : Komnenos.Commands.Komnenos_Command)
   is
      pragma Unreferenced (Item);
   begin
      Ada.Text_IO.Put_Line (Komnenos.Commands.Show (Command));
   end Execute_Command;

   ------------
   -- Exists --
   ------------

   overriding function Exists
     (Table : Entity_Table;
      Key   : String)
      return Boolean
   is
   begin
      return Table.Map.Contains (Key);
   end Exists;

   -----------------
   -- File_Column --
   -----------------

   function File_Column
     (Location : File_Location)
      return Natural
   is
   begin
      return Location.Column;
   end File_Column;

   ---------------
   -- File_Line --
   ---------------

   function File_Line
     (Location : File_Location)
      return Natural
   is
   begin
      return Location.Line;
   end File_Line;

   ----------
   -- Find --
   ----------

   overriding function Find
     (Table      : Entity_Table;
      Name       : String;
      Class_Name : String)
      return Entity_Reference
   is
   begin
      if Table.Name_Map.Contains (Name) then
         for Item of Table.Name_Map (Name) loop
            if Class_Name = ""
              or else Class (Item.all) = Class_Name
            then
               return Item;
            end if;
         end loop;
      end if;
      return null;
   end Find;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Table : Entity_Table;
      Key   : String)
      return Entity_Reference
   is
   begin
      return Table.Map.Element (Key);
   end Get;

   ----------------
   -- Get_Cursor --
   ----------------

   function Get_Cursor
     (Item : Root_Entity_Reference)
      return Aquarius.Layout.Position
   is
   begin
      return Item.Cursor;
   end Get_Cursor;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (File_Name : String;
                     Line      : Natural;
                     Column    : Natural)
                     return String
   is
   begin
      return File_Name & Integer'Image (-Line)
        & Integer'Image (-Column);
   end Get_Key;

   -------------------
   -- Get_Reference --
   -------------------

   function Get_Reference
     (Table    : Entity_Table_Interface'Class;
      Location : File_Location)
      return Entity_Reference
   is
      use type Aquarius.Programs.Program_Tree;
      File_Name : constant String :=
                    Table.Location_File_Name (Location);
      Program   : constant Aquarius.Programs.Program_Tree :=
                    Table.Program_Store.Get_Program (File_Name);
      Child     : constant Aquarius.Programs.Program_Tree :=
                    Program.Find_Node_At
                      ((Aquarius.Layout.Count (Location.Line),
                       Aquarius.Layout.Count (Location.Column)));
      Declaration : constant Aquarius.Programs.Program_Tree :=
                      Child.Declaration_Parent;
      Key       : constant String :=
                     Get_Key (File_Name,
                              Declaration.Location_Line,
                              Declaration.Location_Column);
   begin
      return Table.Get (Key);
   end Get_Reference;

   ----------------
   -- Identifier --
   ----------------

   function Identifier
     (Item : Root_Entity_Reference'Class)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Identifier);
   end Identifier;

   -------------
   -- Iterate --
   -------------

   overriding procedure Iterate
     (Table   : Entity_Table;
      Filter  : in String;
      Process : not null access
        procedure (Item : Entity_Reference);
      Top_Level_Only : Boolean := True)
   is
      Filter_Text : constant String :=
                      Ada.Characters.Handling.To_Lower (Filter);
   begin
      if Filter = "" then
         for Entity of Table.Table loop
            if not Top_Level_Only or else Entity.Top_Level then
               Process (Entity);
            end if;
         end loop;
      else
         for Entity of Table.Table loop
            if not Top_Level_Only or else Entity.Top_Level then
               declare
                  Match_Text : constant String :=
                                 Ada.Characters.Handling.To_Lower
                                   (Ada.Strings.Unbounded.To_String
                                      (Entity.Identifier));
               begin
                  if Ada.Strings.Fixed.Index
                    (Match_Text, Filter_Text)
                    > 0
                  then
                     Process (Entity);
                  end if;
               end;
            end if;
         end loop;
      end if;
   end Iterate;

   ---------
   -- Key --
   ---------

   function Key
     (Item : Root_Entity_Reference)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Key);
   end Key;

   ------------------------
   -- Location_File_Name --
   ------------------------

   overriding function Location_File_Name
     (Table : Entity_Table;
      Location : File_Location)
      return String
   is
   begin
      return Table.File_Vector (Location.File);
   end Location_File_Name;

   -----------------------------
   -- Location_Reference_Type --
   -----------------------------

   function Location_Reference_Type
     (Location : File_Location)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Location.Ref_Type);
   end Location_Reference_Type;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Item : Root_Entity_Reference) return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Identifier);
   end Name;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Visual : in out Entity_Visual'Class;
      Text   : in     String;
      Style  : in     Aquarius.Styles.Aquarius_Style;
      Link   : access Root_Entity_Reference'Class := null)
   is
   begin
      Visual.Put (Text, Style, Link);
      Visual.New_Line;
   end Put_Line;

   ----------------
   -- References --
   ----------------

   overriding function References
     (Table  : Entity_Table;
      Entity : Entity_Reference)
      return File_Location_Array
   is
      pragma Unreferenced (Table);
      Result : File_Location_Array
        (1 .. Natural (Entity.References.Last_Index));
   begin
      for I in Result'Range loop
         Result (I) := Entity.References (File_Id (I));
      end loop;
      return Result;
   end References;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor
     (Item     : in out Root_Entity_Reference;
      Position : Aquarius.Layout.Position)
   is
   begin
      Item.Cursor := Position;
   end Set_Cursor;

   -----------------------
   -- Set_Program_Store --
   -----------------------

   overriding procedure Set_Program_Store
     (Table : in out Entity_Table;
      Store : access Program_Store_Interface'Class)
   is
   begin
      Table.Store := Store;
   end Set_Program_Store;

   ----------
   -- Show --
   ----------

   overriding function Show
     (Item : Root_Entity_Reference) return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Description);
   end Show;

   ----------
   -- Sort --
   ----------

   overriding procedure Sort
     (Table   : in out Entity_Table)
   is
   begin
      Entity_Sorting.Sort (Table.Table);
   end Sort;

   ----------
   -- Text --
   ----------

   overriding function Text
     (Item : Root_Entity_Reference) return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Display_Text);
   end Text;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Table : Entity_Table_Interface'Class;
      Location : File_Location)
      return String
   is
      use Ada.Strings, Ada.Strings.Fixed;
   begin
      return Table.Location_File_Name (Location)
        & ":" & Trim (Natural'Image (Location.Line), Left)
        & ":" & Trim (Natural'Image (Location.Column), Left)
        & " [" & Ada.Strings.Unbounded.To_String (Location.Ref_Type) & "]";
   end To_String;

end Komnenos.Entities;
