with Ada.Text_IO;

with Tagatha.Temporaries;

package body Tagatha.Registry is

   procedure Record_Pop (Register : in out Tagatha_Registry;
                         Size     : in     Tagatha_Size;
                         Operand  : in     Transfers.Transfer_Operand);

   ------------
   -- Append --
   ------------

   procedure Append (Register : in out Tagatha_Registry;
                     T        : in     Tagatha.Transfers.Transfer)
   is
   begin
      Register.Labels.Append (Register.Last_Label);
      Register.Last_Label := Tagatha.Labels.No_Label;
      Register.Transfers.Append (T);
   end Append;

   -------------------
   -- Get_Transfers --
   -------------------

   function Get_Transfers (Register : in Tagatha_Registry)
                          return Tagatha.Transfers.Array_Of_Transfers
   is
      Result : Transfers.Array_Of_Transfers
        (1 .. Natural (Register.Transfers.Length) + 2);
   begin
      for I in Result'Range loop
         if I = 1 then
            Result (I) :=
              Tagatha.Transfers.Reserve_Stack (Register.Frame_Size);
         elsif I = Result'Last then
            Result (I) :=
              Tagatha.Transfers.Restore_Stack (Register.Frame_Size);
         else
            Result (I) := Register.Transfers.Element (I - 1);
            Tagatha.Transfers.Set_Label (Result (I),
                                         Register.Labels.Element (I - 1));
         end if;
         Ada.Text_IO.Put_Line (Tagatha.Transfers.Show (Result (I)));
      end loop;
      return Result;
   end Get_Transfers;

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
   -- Record_Jump --
   -----------------

   procedure Record_Jump (Register    : in out Tagatha_Registry;
                          Condition   : in     Tagatha_Condition;
                          Destination : in     Tagatha.Labels.Tagatha_Label)
   is
   begin
      Ada.Text_IO.Put_Line ("jump " & Condition'Img &
                              " " & Tagatha.Labels.Show (Destination, 'L'));
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

   ----------------------
   -- Record_Operation --
   ----------------------

   procedure Record_Operation (Register : in out Tagatha_Registry;
                               Operator : in     Tagatha_Operator)
   is
      Top : constant Natural := Register.Stack.Last_Index;
      use Tagatha.Expressions;
   begin
      Ada.Text_IO.Put_Line (Operator'Img);
      if Operator in Zero_Argument_Operator then
         null;
      elsif Operator in One_Argument_Operator then
         Register.Stack.Replace_Element
           (Top,
            New_Operator_Expression (Operator,
                                     Register.Stack.Element (Top)));
      else
         Register.Stack.Replace_Element
           (Top - 1,
            New_Operator_Expression (Operator,
                                     Register.Stack.Element (Top - 1),
                                     Register.Stack.Element (Top)));
         Register.Stack.Delete (Top);
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
      Ada.Text_IO.Put_Line ("pop " &
                              Positive'Image (Register.Stack.Last_Index) &
                              " " &
                              Tagatha.Operands.Show (Operand));
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
      Temps     : constant Tagatha.Temporaries.Temporary_Source :=
        Tagatha.Temporaries.New_Source;
      Transfers : Tagatha.Transfers.Array_Of_Transfers :=
        Tagatha.Expressions.Get_Transfers
        (Temps,
         Register.Stack.Element (Register.Stack.Last_Index),
         Operand);
   begin
      for I in Transfers'Range loop
         Register.Append (Transfers (I));
      end loop;
      Tagatha.Transfers.Set_Size
        (Transfers (Transfers'Last), Size);
      Register.Stack.Delete (Register.Stack.Last_Index);
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
      Ada.Text_IO.Put_Line ("push" &
                              Positive'Image (Register.Stack.Last_Index) &
                              " " &
                              Tagatha.Operands.Show (Operand));
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
   end Start;

end Tagatha.Registry;
