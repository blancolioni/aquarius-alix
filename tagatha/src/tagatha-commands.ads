with Tagatha.Labels;
with Tagatha.Operands;
with Tagatha.Registry;

package Tagatha.Commands is

   type Tagatha_Command is private;

   function Push (Operand    : Tagatha.Operands.Tagatha_Operand;
                  Size       : Tagatha_Size := Default_Integer_Size)
                  return Tagatha_Command;

   function Pop  (Operand    : Tagatha.Operands.Tagatha_Operand;
                  Size       : Tagatha_Size     := Default_Integer_Size)
                  return Tagatha_Command;

   function Operate (Op   : Tagatha_Operator;
                     Neg  : Boolean           := False;
                     Size : Tagatha_Size      := Default_Integer_Size)
                     return Tagatha_Command;

   function Call (Target : Tagatha.Labels.Tagatha_Label)
                 return Tagatha_Command;

   function Loop_Around (Label        : Tagatha.Labels.Tagatha_Label;
                         Loop_Count   : Local_Offset;
                         Loop_Index   : Local_Offset;
                         Size         : Tagatha_Size := Default_Integer_Size)
                        return Tagatha_Command;

   function Jump (Target : Tagatha.Labels.Tagatha_Label;
                  Cond   : Tagatha_Condition := C_Always;
                  Size   : Tagatha_Size      := Default_Address_Size)
                 return Tagatha_Command;

   procedure Register_Command
     (Register  : in out Tagatha.Registry.Tagatha_Registry;
      Command   : in     Tagatha_Command);

   function Show (Command : Tagatha_Command) return String;

   procedure Set_Label (Command : in Tagatha_Command;
                        Label   : in Tagatha.Labels.Tagatha_Label);

   function Get_Label (Command : in Tagatha_Command)
                      return Tagatha.Labels.Tagatha_Label;

private

   type Stack_Operation is (S_Push, S_Pop);

   type Tagatha_Instruction is
     (T_Stack,     --  push or pop

      T_Operate,   --  operate on stack elements (e.g. add/subtract)

      T_Call,      --  call another unit

      T_Loop,      --  bounded loop

      T_Jump       --  local jump within this unit
     );

   type Tagatha_Command_Record
     (Instruction : Tagatha_Instruction := T_Stack) is
      record
         Size   : Tagatha_Size;
         Label  : Tagatha.Labels.Tagatha_Label;
         Negate : Boolean;
         case Instruction is
            when T_Stack =>
               Stack_Op    : Stack_Operation;
               Operand     : Tagatha.Operands.Tagatha_Operand;
            when T_Operate =>
               Operator    : Tagatha_Operator;
            when T_Call =>
               Subroutine  : Tagatha.Labels.Tagatha_Label;
            when T_Loop =>
               Limit       : Local_Offset;
               Counter     : Local_Offset;
               End_Label   : Tagatha.Labels.Tagatha_Label;
            when T_Jump =>
               Condition   : Tagatha_Condition;
               Destination : Tagatha.Labels.Tagatha_Label;
         end case;
      end record;

   type Tagatha_Command is access Tagatha_Command_Record;

   function Get_Command_Operator (Command : Tagatha_Command)
                                  return Tagatha_Operator;

   function Get_Jump_Condition (Command : Tagatha_Command)
                                return Tagatha_Condition;

   function Get_Stack_Operand (Command : Tagatha_Command)
                               return Tagatha.Operands.Tagatha_Operand;

   function Get_Stack_Operation (Command : Tagatha_Command)
                                 return Stack_Operation;

end Tagatha.Commands;
