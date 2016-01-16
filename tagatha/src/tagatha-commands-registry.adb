package body Tagatha.Commands.Registry is

   ----------------------
   -- Register_Command --
   ----------------------

   procedure Register_Command
     (Register  : in out Tagatha.Registry.Tagatha_Registry;
      Command   : in     Tagatha_Command)
   is
   begin
      Register.Record_Label (Get_Label (Command));
      case Command.Instruction is
         when T_Stack =>
            case Command.Stack_Op is
               when S_Push =>
                  Register.Record_Push (Command.Size, Command.Operand);
               when S_Pop =>
                  Register.Record_Pop (Command.Size, Command.Operand);
               when S_Drop =>
                  Register.Record_Drop (Command.Size);
            end case;
         when T_Operate =>
            Register.Record_Operation (Command.Operator);
         when T_Call =>
            Register.Record_Call (Command.Subroutine);
         when T_Loop =>
            Register.Record_Loop (Command.Limit,
                                  Command.Counter, Command.End_Label);
         when T_Jump =>
            Register.Record_Jump (Command.Condition,
                                  Command.Destination);
         when T_Native =>
            Register.Record_Native_Operation
              (Ada.Strings.Unbounded.To_String (Command.Native_Name),
               Ada.Strings.Unbounded.To_String (Command.Changed_Registers),
               Command.Input_Words, Command.Output_Words);
      end case;
   end Register_Command;

end Tagatha.Commands.Registry;