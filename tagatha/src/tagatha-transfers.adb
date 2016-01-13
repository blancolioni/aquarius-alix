package body Tagatha.Transfers is

   function Show_Operator (Item : Tagatha_Operator) return String;

   ----------------------
   -- Argument_Operand --
   ----------------------

   function Argument_Operand (Arg_Index     : Argument_Offset)
                              return Transfer_Operand
   is
   begin
      return (T_Argument, No_Modification, Arg_Index);
   end Argument_Operand;

   -----------------------
   -- Condition_Operand --
   -----------------------

   function Condition_Operand return Transfer_Operand is
   begin
      return (T_Condition, No_Modification);
   end Condition_Operand;

   ----------------------
   -- Constant_Operand --
   ----------------------

   function Constant_Operand (Value : Tagatha.Constants.Tagatha_Constant)
                              return Transfer_Operand
   is
   begin
      return (T_Immediate, No_Modification, Value);
   end Constant_Operand;

   ----------------------
   -- Control_Transfer --
   ----------------------

   function Control_Transfer (Condition   : Tagatha_Condition;
                              Destination : Tagatha.Labels.Tagatha_Label)
                             return Transfer
   is
   begin
      return (Trans       => T_Control,
              Reserve     => 0,
              Label       => Tagatha.Labels.No_Label,
              Condition   => Condition,
              Destination => Destination,
              Self        => False,
              Src_1       => Null_Operand,
              Src_2       => Null_Operand,
              Dst         => Null_Operand,
              Op          => Op_Nop);
   end Control_Transfer;

   --------------------
   -- Get_Arg_Offset --
   --------------------

   function Get_Arg_Offset (Item : Transfer_Operand) return Argument_Offset is
   begin
      return Item.Arg_Offset;
   end Get_Arg_Offset;

   -------------------
   -- Get_Condition --
   -------------------

   function Get_Condition (T : Transfer) return Tagatha_Condition is
   begin
      return T.Condition;
   end Get_Condition;

   ---------------------
   -- Get_Destination --
   ---------------------

   function Get_Destination (T : Transfer)
                            return Tagatha.Labels.Tagatha_Label
   is
   begin
      return T.Destination;
   end Get_Destination;

   ---------------------
   -- Get_Destination --
   ---------------------

   function Get_Destination (Item : Transfer) return Transfer_Operand is
   begin
      return Item.Dst;
   end Get_Destination;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (T : Transfer)
                      return Tagatha.Labels.Tagatha_Label
   is
   begin
      return T.Label;
   end Get_Label;

   ----------------------
   -- Get_Local_Offset --
   ----------------------

   function Get_Local_Offset (Item : Transfer_Operand) return Local_Offset is
   begin
      return Item.Loc_Offset;
   end Get_Local_Offset;

   ------------------
   -- Get_Operator --
   ------------------

   function Get_Operator (Item : Transfer) return Tagatha_Operator is
   begin
      return Item.Op;
   end Get_Operator;

   ---------------------
   -- Get_Reservation --
   ---------------------

   function Get_Reservation (T : Transfer) return Integer is
   begin
      return T.Reserve;
   end Get_Reservation;

   --------------
   -- Get_Size --
   --------------

   function Get_Size (Item : Transfer_Operand) return Tagatha_Size is
   begin
      return Item.Modifiers.Size;
   end Get_Size;

   --------------------------
   -- Get_Slice_Bit_Length --
   --------------------------

   function Get_Slice_Bit_Length  (Item : Transfer_Operand)
                                  return Tagatha_Integer
   is
   begin
      return Item.Modifiers.Slice.Last_Bit -
        Item.Modifiers.Slice.First_Bit + 1;
   end Get_Slice_Bit_Length;

   --------------------------
   -- Get_Slice_Bit_Offset --
   --------------------------

   function Get_Slice_Bit_Offset  (Item : Transfer_Operand)
                                  return Tagatha_Integer
   is
   begin
      if Item.Modifiers.Have_Slice then
         return Item.Modifiers.Slice.First_Bit;
      else
         return 0;
      end if;
   end Get_Slice_Bit_Offset;

   --------------------
   -- Get_Slice_Mask --
   --------------------

   function Get_Slice_Mask (Item : Transfer_Operand)
                           return Tagatha_Integer is
   begin
      if not Item.Modifiers.Have_Slice then
         return Tagatha_Integer'Last;
      else
         return (2**Integer (Get_Slice_Bit_Length (Item)) - 1) *
           Get_Slice_Bit_Offset (Item);
      end if;
   end Get_Slice_Mask;

   ----------------------------
   -- Get_Slice_Octet_Length --
   ----------------------------

   function Get_Slice_Octet_Length (Item : Transfer_Operand)
                                  return Tagatha_Integer
   is
      Bit_Length : Tagatha_Integer := Get_Slice_Bit_Length (Item);
   begin
      if Bit_Length mod 8 /= 0 then
         Bit_Length := Bit_Length + (8 - Bit_Length mod 8);
      end if;
      return Bit_Length;
   end Get_Slice_Octet_Length;

   ----------------------------
   -- Get_Slice_Octet_Offset --
   ----------------------------

   function Get_Slice_Octet_Offset (Item : Transfer_Operand)
                                  return Tagatha_Integer
   is
      Bit_Offset : constant Tagatha_Integer := Get_Slice_Bit_Offset (Item);
   begin
      return Bit_Offset - Bit_Offset mod 8;
   end Get_Slice_Octet_Offset;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source (Item : Transfer) return Transfer_Operand is
   begin
      return Get_Source_1 (Item);
   end Get_Source;

   ------------------
   -- Get_Source_1 --
   ------------------

   function Get_Source_1 (Item : Transfer) return Transfer_Operand is
   begin
      return Item.Src_1;
   end Get_Source_1;

   ------------------
   -- Get_Source_2 --
   ------------------

   function Get_Source_2 (Item : Transfer) return Transfer_Operand is
   begin
      return Item.Src_2;
   end Get_Source_2;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Item : Transfer_Operand)
                      return Tagatha.Constants.Tagatha_Constant
   is
   begin
      return Item.Value;
   end Get_Value;

   ------------------
   -- Has_Operator --
   ------------------

   function Has_Operator (Item : Transfer) return Boolean is
   begin
      return Item.Op /= Op_Nop;
   end Has_Operator;

   --------------
   -- Has_Size --
   --------------

   function Has_Size (Item : in Transfer_Operand) return Boolean is
   begin
      return Item.Modifiers.Have_Size;
   end Has_Size;

   ---------------
   -- Has_Slice --
   ---------------

   function Has_Slice (Item : Transfer_Operand) return Boolean is
   begin
      return Item.Modifiers.Have_Slice;
   end Has_Slice;

   -----------------
   -- Is_Argument --
   -----------------

   function Is_Argument (Item : Transfer_Operand) return Boolean is
   begin
      return Item.Op = T_Argument;
   end Is_Argument;

   -----------------
   -- Is_Constant --
   -----------------

   function Is_Constant (Item : Transfer_Operand) return Boolean is
   begin
      return Item.Op = T_Immediate;
   end Is_Constant;

   ----------------------
   -- Is_Constant_Zero --
   ----------------------

   function Is_Constant_Zero (Item : Transfer_Operand) return Boolean is
   begin
      return Item.Op = T_Immediate and then
        Tagatha.Constants.Is_Integer (Item.Value) and then
        Tagatha.Constants.Get_Integer (Item.Value) = 0;
   end Is_Constant_Zero;

   ----------------
   -- Is_Control --
   ----------------

   function Is_Control (T : Transfer) return Boolean is
   begin
      return T.Trans = T_Control;
   end Is_Control;

   --------------------------
   -- Is_Frame_Reservation --
   --------------------------

   function Is_Frame_Reservation (T : Transfer) return Boolean is
   begin
      return T.Trans = T_Change_Stack;
   end Is_Frame_Reservation;

   --------------
   -- Is_Local --
   --------------

   function Is_Local    (Item : Transfer_Operand) return Boolean is
   begin
      return Item.Op = T_Local;
   end Is_Local;

   ---------------
   -- Is_Result --
   ---------------

   function Is_Result   (Item : Transfer_Operand) return Boolean is
   begin
      return Item.Op = T_Result;
   end Is_Result;

   ---------------
   -- Is_Simple --
   ---------------

   function Is_Simple    (Item : Transfer) return Boolean is
   begin
      return Item.Op = Op_Nop;
   end Is_Simple;

   -------------------
   -- Label_Operand --
   -------------------

   function Label_Operand (Label         : Tagatha.Labels.Tagatha_Label)
                           return Transfer_Operand
   is
   begin
      return (T_Immediate, No_Modification,
              Constants.Label_Constant (Label));
   end Label_Operand;

   -------------------
   -- Local_Operand --
   -------------------

   function Local_Operand (Frame_Index   : Local_Offset)
                           return Transfer_Operand
   is
   begin
      return (T_Local, No_Modification, Frame_Index);
   end Local_Operand;

   ------------------------
   -- Operation_Transfer --
   ------------------------

   function Operation_Transfer (Src_1         : Transfer_Operand;
                                Src_2         : Transfer_Operand;
                                Op            : Tagatha_Operator;
                                To            : Transfer_Operand)
                                return Transfer
   is
   begin
      return (Trans       => T_Data,
              Reserve     => 0,
              Label       => Tagatha.Labels.No_Label,
              Condition   => C_Always,
              Destination => Tagatha.Labels.No_Label,
              Self        => Same_Operand (Src_1, To),
              Src_1       => Src_1,
              Src_2       => Src_2,
              Dst         => To,
              Op          => Op);
   end Operation_Transfer;

   -------------------
   -- Reserve_Stack --
   -------------------

   function Reserve_Stack (Frame_Size : Natural) return Transfer is
      Result : Transfer;
   begin
      Result.Trans   := T_Change_Stack;
      Result.Reserve := Frame_Size;
      return Result;
   end Reserve_Stack;

   -------------------
   -- Restore_Stack --
   -------------------

   function Restore_Stack (Frame_Size : Natural) return Transfer is
      Result : Transfer;
   begin
      Result.Trans   := T_Change_Stack;
      Result.Reserve := -Frame_Size;
      return Result;
   end Restore_Stack;

   --------------------
   -- Result_Operand --
   --------------------

   function Result_Operand return Transfer_Operand is
   begin
      return (T_Result, No_Modification);
   end Result_Operand;

   ------------------
   -- Same_Operand --
   ------------------

   function Same_Operand (Left, Right : Transfer_Operand) return Boolean is
      use type Tagatha.Constants.Tagatha_Constant;
      use type Tagatha.Labels.Tagatha_Label;
      use type Tagatha.Temporaries.Temporary;
   begin
      if Left.Op /= Right.Op then
         return False;
      else
         case Left.Op is
            when T_No_Operand =>
               return True;
            when T_Stack =>
               return True;
            when T_Condition =>
               return True;
            when T_Temporary =>
               return Left.Temp = Right.Temp;
            when T_Local =>
               return Left.Loc_Offset = Right.Loc_Offset;
            when T_Argument =>
               return Left.Arg_Offset = Right.Arg_Offset;
            when T_Result =>
               return True;
            when T_Immediate =>
               return Left.Value = Right.Value;
         end case;
      end if;
   end Same_Operand;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label (T       : in out Transfer;
                        Label   : in     Tagatha.Labels.Tagatha_Label)
   is
   begin
      T.Label := Label;
   end Set_Label;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size (Item : in out Transfer_Operand;
                       Size : in     Tagatha_Size)
   is
   begin
      Item.Modifiers.Have_Size := True;
      Item.Modifiers.Size     := Size;
   end Set_Size;

   procedure Set_Size (Item : in out Transfer;
                       Size : in Tagatha_Size)
   is
   begin
      Set_Size (Item.Dst, Size);
   end Set_Size;

   ----------
   -- Show --
   ----------

   function Show (Item : Transfer) return String is
   begin
      case Item.Trans is
         when T_Change_Stack =>
            if Item.Reserve < 0 then
               return "close_frame" & Integer'Image (-Item.Reserve);
            else
               return "open_frame" & Integer'Image (Item.Reserve);
            end if;
         when T_Control =>
            return "jump " & Tagatha_Condition'Image (Item.Condition) &
              " " & Tagatha.Labels.Show (Item.Destination, 'L');
         when T_Data =>
            declare
               Dst    : constant String := Show (Item.Dst);
               Src_1  : constant String := Show (Item.Src_1);
               Src_2  : constant String := Show (Item.Src_2);
               Op     : constant String := Show_Operator (Item.Op);
               Sz     : Character;
            begin
               case Item.Dst.Modifiers.Size is
                  when Size_8 =>
                     Sz := '1';
                  when Size_16 =>
                     Sz := '2';
                  when Size_32 =>
                     Sz := '4';
                  when Size_64 =>
                     Sz := '8';
                  when Default_Size =>
                     Sz := 'd';
                  when Default_Integer_Size =>
                     Sz := 'i';
                  when Default_Address_Size =>
                     Sz := 'a';
               end case;

               if Item.Op = Op_Nop then
                  return Sz & ':' & Dst & " := " & Src_1;
               else
                  return Sz & ':' & Dst & " := " &
                  Src_1 & " " & Op & " " & Src_2;
               end if;
            end;
      end case;
   end Show;

   ----------
   -- Show --
   ----------

   function Show (Item : Transfer_Operand) return String is
   begin
      case Item.Op is
         when T_No_Operand =>
            return "<>";
         when T_Stack =>
            return "<stack>";
         when T_Condition =>
            return "<cc>";
         when T_Temporary =>
            return Temporaries.Show (Item.Temp);
         when T_Argument =>
            return "arg" &
              Integer'Image (-1 * Integer (Item.Arg_Offset));
         when T_Result =>
            return "result";
         when T_Local =>
            return "frm" &
            Integer'Image (-1 * Integer (Item.Loc_Offset));
         when T_Immediate =>
            return Tagatha.Constants.Show (Item.Value);
      end case;

   end Show;

   -------------------
   -- Show_Operator --
   -------------------

   function Show_Operator (Item : Tagatha_Operator) return String is
   begin
      return Tagatha_Operator'Image (Item);
   end Show_Operator;

   ---------------------
   -- Simple_Transfer --
   ---------------------

   function Simple_Transfer (From : Transfer_Operand;
                             To   : Transfer_Operand)
                             return Transfer
   is
   begin
      return (Trans       => T_Data,
              Reserve     => 0,
              Label       => Tagatha.Labels.No_Label,
              Condition   => C_Always,
              Destination => Tagatha.Labels.No_Label,
              Self        => Same_Operand (From, To),
              Src_1       => From,
              Src_2       => Null_Operand,
              Dst         => To,
              Op          => Op_Nop);
   end Simple_Transfer;

   -----------
   -- Slice --
   -----------

   function Slice (Item   : Transfer_Operand;
                   Offset : Natural;
                   Size   : Tagatha_Size)
                  return Transfer_Operand
   is
      Result : Transfer_Operand := Item;
   begin
      Result.Modifiers.Have_Slice := True;
      Result.Modifiers.Slice      :=
        (Tagatha_Integer (Offset * Size_Bits (Size)),
         Tagatha_Integer ((Offset + 1) * Size_Bits (Size) - 1));
      return Result;
   end Slice;

   ----------------
   -- Slice_Fits --
   ----------------

   function Slice_Fits (Item : Transfer_Operand;
                        Size : Tagatha_Size)
                       return Boolean
   is
   begin
      return Get_Slice_Bit_Length (Item) <= 2**(Tagatha_Size'Pos (Size) + 3);
   end Slice_Fits;

   ----------------------
   -- Temprary_Operand --
   ----------------------

   function Temporary_Operand (Temp          : Tagatha.Temporaries.Temporary)
                               return Transfer_Operand
   is
   begin
      return (T_Temporary, No_Modification, Temp);
   end Temporary_Operand;

   ------------------
   -- To_Temporary --
   ------------------

   function To_Temporary (Src_1, Src_2 : Transfer_Operand;
                          Op           : Tagatha_Operator;
                          Dst          : Temporaries.Temporary)
                          return Transfer
   is
   begin
      return (Trans       => T_Data,
              Reserve     => 0,
              Label       => Tagatha.Labels.No_Label,
              Condition   => C_Always,
              Destination => Tagatha.Labels.No_Label,
              Self        => False,
              Src_1       => Src_1,
              Src_2       => Src_2,
              Dst         => (T_Temporary, No_Modification, Dst),
              Op          => Op);
   end To_Temporary;

   -----------------
   -- To_Transfer --
   -----------------

   function To_Transfer (Op   : Tagatha.Operands.Tagatha_Operand)
                        return Transfer_Operand
   is
      use Tagatha.Operands;
   begin
      if Is_Constant (Op) then
         return Constant_Operand (Get_Value (Op));
      elsif Is_Argument (Op) then
         return Argument_Operand (Get_Arg_Offset (Op));
      elsif Is_Local (Op) then
         return Local_Operand (Get_Local_Offset (Op));
      elsif Is_Result (Op) then
         return Result_Operand;
      else
         raise Constraint_Error with
           "unknown operand type: " & Show (Op);
      end if;
   end To_Transfer;

   -----------------
   -- To_Transfer --
   -----------------

   function To_Transfer (Op   : Tagatha.Operands.Tagatha_Operand;
                         Size : Tagatha_Size)
                        return Transfer_Operand
   is
      Result : Transfer_Operand;
   begin
      Result := To_Transfer (Op);
      Set_Size (Result, Size);
      return Result;
   end To_Transfer;

end Tagatha.Transfers;
