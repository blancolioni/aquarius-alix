package body Tagatha.Commands is

   ----------
   -- Call --
   ----------

   function Call (Target : Tagatha.Labels.Tagatha_Label)
                  return Tagatha_Command
   is
   begin
      return new Tagatha_Command_Record'(T_Call, Default_Address_Size,
                                         Tagatha.Labels.No_Label, False,
                                         Target);
   end Call;

   --------------------------
   -- Get_Command_Operator --
   --------------------------

   function Get_Command_Operator (Command : Tagatha_Command)
                                  return Tagatha_Operator
   is
   begin
      return Command.Operator;
   end Get_Command_Operator;

   ------------------------
   -- Get_Jump_Condition --
   ------------------------

   function Get_Jump_Condition (Command : Tagatha_Command)
                                return Tagatha_Condition
   is
   begin
      return Command.Condition;
   end Get_Jump_Condition;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Command : in Tagatha_Command)
                      return Tagatha.Labels.Tagatha_Label
   is
   begin
      return Command.Label;
   end Get_Label;

   -----------------------
   -- Get_Stack_Operand --
   -----------------------

   function Get_Stack_Operand (Command : Tagatha_Command)
                               return Tagatha.Operands.Tagatha_Operand
   is
   begin
      return Command.Operand;
   end Get_Stack_Operand;

   -------------------------
   -- Get_Stack_Operation --
   -------------------------

   function Get_Stack_Operation (Command : Tagatha_Command)
                                 return Stack_Operation
   is
   begin
      return Command.Stack_Op;
   end Get_Stack_Operation;

   ----------
   -- Jump --
   ----------

   function Jump (Target : Tagatha.Labels.Tagatha_Label;
                  Cond   : Tagatha_Condition := C_Always;
                  Size   : Tagatha_Size      := Default_Address_Size)
                  return Tagatha_Command
   is
   begin
      return new Tagatha_Command_Record'(T_Jump, Size,
                                         Tagatha.Labels.No_Label,
                                         False, Cond, Target);
   end Jump;

   -----------------
   -- Loop_Around --
   -----------------

   function Loop_Around (Label        : Tagatha.Labels.Tagatha_Label;
                         Loop_Count   : Local_Offset;
                         Loop_Index   : Local_Offset;
                         Size         : Tagatha_Size := Default_Integer_Size)
                         return Tagatha_Command
   is
   begin
      return new Tagatha_Command_Record'(T_Loop, Size,
                                         Tagatha.Labels.No_Label,
                                         False,
                                         Loop_Count, Loop_Index, Label);
   end Loop_Around;

   -------------
   -- Operate --
   -------------

   function Operate (Op   : Tagatha_Operator;
                     Neg  : Boolean           := False;
                     Size : Tagatha_Size      := Default_Integer_Size)
                     return Tagatha_Command
   is
   begin
      return new Tagatha_Command_Record'(T_Operate, Size,
                                         Tagatha.Labels.No_Label, Neg, Op);
   end Operate;

   ---------
   -- Pop --
   ---------

   function Pop (Operand    : Tagatha.Operands.Tagatha_Operand;
                 Size       : Tagatha_Size     := Default_Integer_Size)
                 return Tagatha_Command
   is
   begin
      return new Tagatha_Command_Record'(T_Stack, Size,
                                         Tagatha.Labels.No_Label,
                                         False,
                                         S_Pop, Operand);
   end Pop;

   ----------
   -- Push --
   ----------

   function Push (Operand    : Tagatha.Operands.Tagatha_Operand;
                  Size       : Tagatha_Size     := Default_Integer_Size)
                  return Tagatha_Command
   is
   begin
      return new Tagatha_Command_Record'(T_Stack, Size,
                                         Tagatha.Labels.No_Label,
                                         False,
                                         S_Push, Operand);
   end Push;

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
      end case;
   end Register_Command;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label (Command : in Tagatha_Command;
                        Label   : in Tagatha.Labels.Tagatha_Label)
   is
   begin
      Command.Label := Label;
   end Set_Label;

   ----------
   -- Show --
   ----------

   function Show (Command : Tagatha_Command) return String is
   begin
      case Command.Instruction is
         when T_Stack =>
            case Command.Stack_Op is
               when S_Push =>
                  return "push " & Tagatha.Operands.Show (Command.Operand);
               when S_Pop =>
                  return "pop  " & Tagatha.Operands.Show (Command.Operand);
            end case;
         when T_Operate =>
            return Command.Operator'Img;
         when T_Call =>
            return "call " & Tagatha.Labels.Show (Command.Subroutine, 'L');
         when T_Loop =>
            return "loop" & Command.Limit'Img & Command.Counter'Img &
              " " & Tagatha.Labels.Show (Command.End_Label, 'L');
         when T_Jump =>
            return "jump " & Command.Condition'Img &
              " " & Tagatha.Labels.Show (Command.Destination, 'L');
      end case;
   end Show;

end Tagatha.Commands;
