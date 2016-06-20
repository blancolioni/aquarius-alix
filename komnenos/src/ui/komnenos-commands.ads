with Aquarius.Layout;

package Komnenos.Commands is

   type Komnenos_Command_Type is
     (No_Command,
      Move_Cursor_Command,
      Set_Cursor_Command,
      Insert_Character_Command,
      New_Line_Command);

   type Move_Unit_Type is
     (By_Character,
      By_Token,
      By_Line,
      By_Fragment);

   type Komnenos_Command (Command : Komnenos_Command_Type) is
      record
         case Command is
            when No_Command =>
               null;
            when Move_Cursor_Command =>
               Offset : Integer;
               Units  : Move_Unit_Type;
            when Set_Cursor_Command =>
               New_Position : Aquarius.Layout.Position;
            when Insert_Character_Command =>
               Ch           : Character;
            when New_Line_Command =>
               null;
         end case;
      end record;

   function Show (Command : Komnenos_Command) return String;

   type Command_Reference is private;

   type Command_Table is tagged private;

   function Get_Command
     (Table : Command_Table;
      Name  : String)
      return Komnenos_Command;

   function Get_Command
     (Table     : Command_Table;
      Reference : Command_Reference)
      return Komnenos_Command;

   function Get_Reference
     (Table : Command_Table;
      Name  : String)
      return Command_Reference;

   procedure Insert_Command
     (Table   : in out Command_Table;
      Name    : String;
      Command : Komnenos_Command);

   function Standard_Table return Command_Table;

private

   type Command_Reference is new Natural;

   type Command_Table_Record;

   type Command_Table is tagged
      record
         Table : access Command_Table_Record;
      end record;

end Komnenos.Commands;
