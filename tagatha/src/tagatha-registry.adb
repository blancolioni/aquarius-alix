package body Tagatha.Registry is

   procedure Record_Pop (Register : in out Tagatha_Registry;
                         Size     : in     Tagatha_Size;
                         Operand  : in     Transfers.Transfer_Operand);

   procedure Force
     (Register : in out Tagatha_Registry;
      Size     : in     Tagatha_Size;
      Offset   : in     Natural) with Unreferenced;

   function Pop
     (Register : in out Tagatha_Registry)
      return Tagatha.Expressions.Expression;

   ------------
   -- Append --
   ------------

   procedure Append (Register : in out Tagatha_Registry;
                     T        : in     Tagatha.Transfers.Transfer)
   is
      LT : Tagatha.Transfers.Transfer := T;
   begin
      if Labels.Has_Label (Register.Last_Label) then
         Transfers.Set_Label (LT, Register.Last_Label);
         Register.Last_Label := Tagatha.Labels.No_Label;
      end if;
      Register.Transfers.Append (LT);
   end Append;

   -----------
   -- Force --
   -----------

   procedure Force
     (Register : in out Tagatha_Registry;
      Size     : in     Tagatha_Size;
      Offset   : in     Natural)
   is
      Transfers : Tagatha.Transfers.Array_Of_Transfers :=
        Tagatha.Expressions.Get_Transfers
        (Register.Temps,
         Register.Stack.Element (Register.Stack.Last_Index - Offset),
         Tagatha.Transfers.Stack_Operand);
   begin
      for I in Transfers'Range loop
         Register.Append (Transfers (I));
      end loop;
      if Transfers'Length > 0 then
         Tagatha.Transfers.Set_Size
           (Transfers (Transfers'Last), Size);
      end if;
   end Force;

   -------------------
   -- Get_Transfers --
   -------------------

   procedure Get_Transfers
     (Register  : in Tagatha_Registry;
      Transfers : in out Tagatha.Transfers.Transfer_Vectors.Vector)
   is
      Reserve_Stack : constant Boolean := Register.Frame_Size > 0;
   begin
      Transfers.Clear;
      if Reserve_Stack then
         Transfers.Append
           (Tagatha.Transfers.Reserve_Stack
              (Register.Frame_Size));
      end if;

      for I in 1 .. Register.Transfers.Last_Index loop
         declare
            T : constant Tagatha.Transfers.Transfer :=
                  Register.Transfers (I);
         begin
            Transfers.Append (T);
         end;
      end loop;

      if Labels.Has_Label (Register.Last_Label) then
         declare
            T : Tagatha.Transfers.Transfer :=
                  Tagatha.Transfers.Reserve_Stack (0);
         begin
            Tagatha.Transfers.Set_Label (T, Register.Last_Label);
            Transfers.Append (T);
         end;
      end if;

      if Reserve_Stack then
         Transfers.Append
           (Tagatha.Transfers.Restore_Stack (Register.Frame_Size));
      end if;

   end Get_Transfers;

   ---------
   -- Pop --
   ---------

   function Pop
     (Register : in out Tagatha_Registry)
      return Tagatha.Expressions.Expression
   is
      E : Tagatha.Expressions.Expression;
   begin
      if Register.Stack.Is_Empty then
         E :=
           Tagatha.Expressions.New_Simple_Expression
             (Tagatha.Transfers.Stack_Operand);
      else
         E := Register.Stack.Last_Element;
         Register.Stack.Delete_Last;
      end if;
      return E;
   end Pop;

   -----------------
   -- Record_Call --
   -----------------

   procedure Record_Call (Register   : in out Tagatha_Registry;
                          Subroutine : in     Tagatha.Labels.Tagatha_Label)
   is
      pragma Unreferenced (Register);
      pragma Unreferenced (Subroutine);
   begin
      null;
   end Record_Call;

   -----------------
   -- Record_Drop --
   -----------------

   procedure Record_Drop (Register : in out Tagatha_Registry;
                          Size     : in     Tagatha_Size)
   is
   begin
      Record_Pop (Register, Size, Tagatha.Transfers.Null_Operand);
   end Record_Drop;

   -----------------
   -- Record_Jump --
   -----------------

   procedure Record_Jump (Register    : in out Tagatha_Registry;
                          Condition   : in     Tagatha_Condition;
                          Destination : in     Tagatha.Labels.Tagatha_Label)
   is
   begin
      if Condition /= C_Always then
         Record_Pop (Register, Default_Address_Size,
                     Tagatha.Transfers.Condition_Operand);
      end if;
      Register.Append
        (Tagatha.Transfers.Control_Transfer (Condition, Destination));

   end Record_Jump;

   ------------------
   -- Record_Label --
   ------------------

   procedure Record_Label (Register   : in out Tagatha_Registry;
                           Label      : in     Tagatha.Labels.Tagatha_Label)
   is
      use type Tagatha.Labels.Tagatha_Label;
   begin
      if Label /= Tagatha.Labels.No_Label then
         Tagatha.Labels.Link_To (Label, Register.Last_Label);
         Register.Last_Label := Label;
      end if;
   end Record_Label;

   -----------------
   -- Record_Loop --
   -----------------

   procedure Record_Loop (Register   : in out Tagatha_Registry;
                          Limit      : in     Local_Offset;
                          Counter    : in     Local_Offset;
                          End_Label  : in     Tagatha.Labels.Tagatha_Label)
   is
      pragma Unreferenced (Register);
      pragma Unreferenced (Limit);
      pragma Unreferenced (Counter);
      pragma Unreferenced (End_Label);
   begin
      null;
   end Record_Loop;

   -----------------------------
   -- Record_Native_Operation --
   -----------------------------

   procedure Record_Native_Operation
     (Register          : in out Tagatha_Registry;
      Name              : String;
      Changed_Registers : String;
      Input_Words       : Natural;
      Output_Words      : Natural)
   is
      pragma Unreferenced (Input_Words);
   begin
      for Operand of Register.Stack loop
         declare
            Transfers : constant Tagatha.Transfers.Array_Of_Transfers :=
                          Tagatha.Expressions.Get_Transfers
                            (Register.Temps, Operand,
                             Tagatha.Transfers.Stack_Operand);
         begin
            for I in Transfers'Range loop
               Register.Append (Transfers (I));
            end loop;
         end;
      end loop;

      Register.Stack.Clear;

      Register.Append
        (Tagatha.Transfers.Native_Transfer (Name, Changed_Registers));

      for I in 1 .. Output_Words loop
         Register.Stack.Append
           (Expressions.New_Simple_Expression (Transfers.Stack_Operand));
      end loop;
   end Record_Native_Operation;

   ----------------------
   -- Record_Operation --
   ----------------------

   procedure Record_Operation (Register : in out Tagatha_Registry;
                               Operator : in     Tagatha_Operator)
   is
      use Tagatha.Expressions;
   begin
      if Operator in Zero_Argument_Operator then
         null;
      elsif Operator in One_Argument_Operator then
         declare
            Left : constant Expression := Pop (Register);
         begin
            Register.Stack.Append
              (New_Operator_Expression (Operator, Left));
         end;
      else
         declare
            Left : constant Expression := Pop (Register);
            Right : constant Expression := Pop (Register);
         begin
            Register.Stack.Append
              (New_Operator_Expression (Operator, Left, Right));
         end;
      end if;
   end Record_Operation;

   ----------------
   -- Record_Pop --
   ----------------

   procedure Record_Pop (Register : in out Tagatha_Registry;
                         Size     : in     Tagatha_Size;
                         Operand  : in     Tagatha.Operands.Tagatha_Operand)
   is
   begin
      Record_Pop (Register, Size,
                  Tagatha.Transfers.To_Transfer (Operand));
   end Record_Pop;

   ----------------
   -- Record_Pop --
   ----------------

   procedure Record_Pop (Register : in out Tagatha_Registry;
                         Size     : in     Tagatha_Size;
                         Operand  : in     Transfers.Transfer_Operand)
   is
   begin
      if Register.Stack.Is_Empty then
         Register.Append
           (Tagatha.Transfers.Simple_Transfer
              (From => Tagatha.Transfers.Stack_Operand,
               To   => Operand));
      else
         declare
            Transfers : Tagatha.Transfers.Array_Of_Transfers :=
                          Tagatha.Expressions.Get_Transfers
                            (Register.Temps,
                             Register.Stack.Element
                               (Register.Stack.Last_Index),
                             Operand);
         begin
            for I in Transfers'Range loop
               Register.Append (Transfers (I));
            end loop;
            Tagatha.Transfers.Set_Size
              (Transfers (Transfers'Last), Size);
            Register.Stack.Delete (Register.Stack.Last_Index);
         end;
      end if;
   end Record_Pop;

   -----------------
   -- Record_Push --
   -----------------

   procedure Record_Push (Register : in out Tagatha_Registry;
                          Size     : in     Tagatha_Size;
                          Operand  : in     Tagatha.Operands.Tagatha_Operand)
   is
      use Tagatha.Expressions;
   begin
      Register.Stack.Append
        (New_Simple_Expression (Transfers.To_Transfer (Operand, Size)));
   end Record_Push;

   -----------
   -- Start --
   -----------

   procedure Start (Register    : in out Tagatha_Registry;
                    Unit_Label  : in     Tagatha.Labels.Tagatha_Label;
                    Size        : in     Natural)
   is
   begin
      Register.Unit_Label := Unit_Label;
      Register.Frame_Size := Size;
      Register.Temps := Tagatha.Temporaries.New_Source;
   end Start;

end Tagatha.Registry;
