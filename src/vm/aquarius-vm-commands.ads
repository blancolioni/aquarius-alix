with Aquarius.Programs;

with Aquarius.VM.Values;

package Aquarius.VM.Commands is

   type VM_Command is private;

   function Get_Internal_Name (Command : VM_Command) return String;
   function Get_External_Name (Command : VM_Command) return String;
   function Get_Menu_Path (Command : VM_Command) return String;
   function Get_Description (Command : VM_Command) return String;

   function Is_Enabled (Command         : VM_Command;
                        Current_Program : Aquarius.Programs.Program_Tree)
                       return Boolean;

   function Execute (Command        : VM_Command;
                     Current_Progam : Aquarius.Programs.Program_Tree)
                    return Aquarius.VM.Values.VM_Value;

private

   type VM_Command_Record;

   type VM_Command is access VM_Command_Record;

end Aquarius.VM.Commands;
