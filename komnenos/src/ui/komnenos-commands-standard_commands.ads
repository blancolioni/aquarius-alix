package Komnenos.Commands.Standard_Commands is

   type Command_Table is tagged private;

   function Get_Command
     (Table : Command_Table;
      Name  : String)
      return Root_Komnenos_Command'Class;

   procedure Insert_Command
     (Table   : in out Command_Table;
      Name    : String;
      Command : Root_Komnenos_Command'Class);

   function Standard_Table return Command_Table;

private

   type Command_Table_Record;

   type Command_Table is tagged
      record
         Table : access Command_Table_Record;
      end record;

end Komnenos.Commands.Standard_Commands;
