with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed.Hash_Case_Insensitive;
with Ada.Strings.Fixed.Equal_Case_Insensitive;

package body Komnenos.Commands is

   package Command_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Command_Reference,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   package Command_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Command_Reference, Komnenos_Command);

   type Command_Table_Record is
      record
         Map : Command_Maps.Map;
      end record;

   Local_Standard_Table  : Command_Table := (Table => null);
   Local_Standard_Vector : Command_Vectors.Vector;

   procedure Check_Standard_Table;

   --------------------------
   -- Check_Standard_Table --
   --------------------------

   procedure Check_Standard_Table is

      procedure Cmd (Name    : String;
                     Command : Komnenos_Command);

      ---------
      -- Cmd --
      ---------

      procedure Cmd (Name    : String;
                     Command : Komnenos_Command)
      is
      begin
         Local_Standard_Vector.Append (Command);
         Local_Standard_Table.Table.Map.Insert
           (Name, Local_Standard_Vector.Last_Index);
      end Cmd;

   begin
      if Local_Standard_Table.Table = null then
         Local_Standard_Table.Table := new Command_Table_Record;

         Cmd ("_", (Command => No_Command));
         Cmd ("previous-line", (Move_Cursor_Command, -1, By_Line));
         Cmd ("next-line", (Move_Cursor_Command, 1, By_Line));
         Cmd ("forward-character", (Move_Cursor_Command, 1, By_Character));
         Cmd ("backward-character", (Move_Cursor_Command, -1, By_Character));
         Cmd ("insert-character", (Insert_Character_Command, Character'First));

         for Ch in Character range ' ' .. '~' loop
            Cmd ("insert-character" & Integer'Image (-Character'Pos (Ch)),
                 (Insert_Character_Command, Ch));
         end loop;

      end if;
   end Check_Standard_Table;

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command
     (Table : Command_Table;
      Name  : String)
      return Komnenos_Command
   is
      Space_Index : constant Natural :=
                      Ada.Strings.Fixed.Index (Name, " ");
      Command_Name : constant String :=
                       (if Space_Index = 0 then Name
                        else Name (Name'First .. Space_Index - 1));
      Argument     : constant String :=
                       (if Space_Index = 0 then ""
                        else Name (Space_Index + 1 .. Name'Last));
   begin
      if Table.Table.Map.Contains (Command_Name) then
         declare
            Result : Komnenos_Command :=
                       Table.Get_Command
                         (Table.Table.Map.Element (Command_Name));
         begin
            if Argument /= "" then
               case Result.Command is
                  when No_Command =>
                     null;
                  when Move_Cursor_Command =>
                     null;
                  when Set_Cursor_Command =>
                     null;
                  when Insert_Character_Command =>
                     Result.Ch := Character'Val (Integer'Value (Argument));
               end case;
            end if;
            return Result;
         end;
      else
         return (Command => No_Command);
      end if;
   end Get_Command;

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command
     (Table     : Command_Table;
      Reference : Command_Reference)
      return Komnenos_Command
   is
   begin
      pragma Unreferenced (Table);
      return Local_Standard_Vector (Reference);
   end Get_Command;

   -------------------
   -- Get_Reference --
   -------------------

   function Get_Reference
     (Table : Command_Table;
      Name  : String)
      return Command_Reference
   is
   begin
      Check_Standard_Table;

      if Table.Table.Map.Contains (Name) then
         return Table.Table.Map.Element (Name);
      else
         return 0;
      end if;
   end Get_Reference;

   --------------------
   -- Insert_Command --
   --------------------

   procedure Insert_Command
     (Table   : in out Command_Table;
      Name    : String;
      Command : Komnenos_Command)
   is
   begin
      Check_Standard_Table;
      Local_Standard_Vector.Append (Command);
      Table.Table.Map.Insert (Name, Local_Standard_Vector.Last_Index);
   end Insert_Command;

   ----------
   -- Show --
   ----------

   function Show (Command : Komnenos_Command) return String is
      function Show (X : Integer) return String;
      function Show (Unit : Move_Unit_Type) return String;

      ----------
      -- Show --
      ----------

      function Show (X : Integer) return String is
      begin
         return Ada.Strings.Fixed.Trim (Integer'Image (X), Ada.Strings.Left);
      end Show;

      ----------
      -- Show --
      ----------

      function Show (Unit : Move_Unit_Type) return String is
      begin
         case Unit is
            when By_Character =>
               return "character";
            when By_Token =>
               return "token";
            when By_Line =>
               return "line";
            when By_Fragment =>
               return "fragment";
         end case;
      end Show;

   begin
      case Command.Command is
         when No_Command =>
            return "no-command";
         when Move_Cursor_Command =>
            return "move-cursor "
              & Show (Command.Units)
              & " " & Show (Command.Offset);
         when Set_Cursor_Command =>
            return "set-cursor "
              & Aquarius.Layout.Show (Command.New_Position);
         when Insert_Character_Command =>
            return "insert-character '" & Command.Ch & "'";
      end case;
   end Show;

   --------------------
   -- Standard_Table --
   --------------------

   function Standard_Table return Command_Table is
   begin
      Check_Standard_Table;
      return Local_Standard_Table;
   end Standard_Table;

end Komnenos.Commands;
