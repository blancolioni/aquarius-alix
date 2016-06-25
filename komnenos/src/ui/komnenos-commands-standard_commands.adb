with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash_Case_Insensitive;
with Ada.Strings.Fixed.Equal_Case_Insensitive;

with Aquarius.Layout;

with Komnenos.Commands.Cursor_Movement;
with Komnenos.Commands.Insert_Delete;
with Komnenos.Commands.Null_Command;

package body Komnenos.Commands.Standard_Commands is

   package Command_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Root_Komnenos_Command'Class,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   type Command_Table_Record is
      record
         Map : Command_Maps.Map;
      end record;

   Local_Standard_Table  : Command_Table := (Table => null);

   procedure Check_Standard_Table;

   --------------------------
   -- Check_Standard_Table --
   --------------------------

   procedure Check_Standard_Table is

      use Aquarius.Layout;

      procedure Cmd
        (Name    : String;
         Command : Root_Komnenos_Command'Class);

      ---------
      -- Cmd --
      ---------

      procedure Cmd
        (Name    : String;
         Command : Root_Komnenos_Command'Class)
      is
      begin
         Local_Standard_Table.Table.Map.Insert
           (Name, Command);
      end Cmd;

   begin
      if Local_Standard_Table.Table = null then
         Local_Standard_Table.Table := new Command_Table_Record;

         Cmd ("_", Null_Command.Null_Command);
         Cmd ("previous-line", Cursor_Movement.Move_By_Line_Command (-1));
         Cmd ("next-line", Cursor_Movement.Move_By_Line_Command (1));
         Cmd ("backward-character",
              Cursor_Movement.Move_By_Character_Command (-1));
         Cmd ("forward-character",
              Cursor_Movement.Move_By_Character_Command (1));

         Cmd ("new-line",
              Insert_Delete.Insert_Character_Command
                (Character'Val (10)));

         for Ch in Character range ' ' .. '~' loop
            Cmd ("insert-character" & Integer'Image (-Character'Pos (Ch)),
                 Insert_Delete.Insert_Character_Command
                   (Ch));
         end loop;

      end if;
   end Check_Standard_Table;

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command
     (Table : Command_Table;
      Name  : String)
      return Root_Komnenos_Command'Class
   is
      Space_Index  : constant Natural :=
                       Ada.Strings.Fixed.Index (Name, " ");
      Command_Name : constant String :=
                       (if Space_Index = 0 then Name
                        else Name (Name'First .. Space_Index - 1));
      Argument     : constant String :=
                       (if Space_Index = 0 then ""
                        else Name (Space_Index + 1 .. Name'Last));
      pragma Unreferenced (Argument);
   begin
      if Table.Table.Map.Contains (Command_Name) then
         return Table.Table.Map.Element (Command_Name);
      else
         return Komnenos.Commands.Null_Command.Null_Command;
      end if;
   end Get_Command;

   --------------------
   -- Insert_Command --
   --------------------

   procedure Insert_Command
     (Table   : in out Command_Table;
      Name    : String;
      Command : Root_Komnenos_Command'Class)
   is
   begin
      Check_Standard_Table;
      Table.Table.Map.Insert (Name, Command);
   end Insert_Command;

   --------------------
   -- Standard_Table --
   --------------------

   function Standard_Table return Command_Table is
   begin
      Check_Standard_Table;
      return Local_Standard_Table;
   end Standard_Table;

end Komnenos.Commands.Standard_Commands;
