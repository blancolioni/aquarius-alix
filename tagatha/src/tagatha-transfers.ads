with Tagatha.Constants;
with Tagatha.Labels;
with Tagatha.Operands;
with Tagatha.Temporaries;

package Tagatha.Transfers is

   type Transfer is private;

   type Transfer_Operand is private;

   Null_Operand : constant Transfer_Operand;

   type Array_Of_Transfers is array (Positive range <>) of Transfer;

   function To_Temporary (Src_1, Src_2 : Transfer_Operand;
                          Op           : Tagatha_Operator;
                          Dst          : Temporaries.Temporary)
                         return Transfer;

   function Constant_Operand (Value : Tagatha.Constants.Tagatha_Constant)
                             return Transfer_Operand;

   function Argument_Operand (Arg_Index     : Argument_Offset)
                              return Transfer_Operand;

   function Local_Operand (Frame_Index   : Local_Offset)
                          return Transfer_Operand;

   function Result_Operand return Transfer_Operand;

   function To_Transfer (Op   : Tagatha.Operands.Tagatha_Operand)
                        return Transfer_Operand;

   function To_Transfer (Op   : Tagatha.Operands.Tagatha_Operand;
                         Size : Tagatha_Size)
                        return Transfer_Operand;

   function Label_Operand (Label         : Tagatha.Labels.Tagatha_Label)
                          return Transfer_Operand;

   function Temporary_Operand (Temp          : Tagatha.Temporaries.Temporary)
                              return Transfer_Operand;

   function Condition_Operand return Transfer_Operand;

   procedure Set_Size (Item : in out Transfer_Operand;
                       Size : in     Tagatha_Size);

   procedure Set_Size (Item : in out Transfer;
                       Size : in     Tagatha_Size);

   function Has_Size (Item : in Transfer_Operand) return Boolean;
   function Get_Size (Item : in Transfer_Operand) return Tagatha_Size;

   function Simple_Transfer (From          : Transfer_Operand;
                             To            : Transfer_Operand)
                            return Transfer;

   function Operation_Transfer (Src_1         : Transfer_Operand;
                                Src_2         : Transfer_Operand;
                                Op            : Tagatha_Operator;
                                To            : Transfer_Operand)
                               return Transfer;

   function Control_Transfer (Condition   : Tagatha_Condition;
                              Destination : Tagatha.Labels.Tagatha_Label)
                             return Transfer;

   function Reserve_Stack (Frame_Size : Natural) return Transfer;
   function Restore_Stack (Frame_Size : Natural) return Transfer;

   procedure Set_Label (T       : in out Transfer;
                        Label   : in     Tagatha.Labels.Tagatha_Label);

   function Get_Label (T : Transfer)
                      return Tagatha.Labels.Tagatha_Label;

   function Is_Frame_Reservation (T : Transfer) return Boolean;
   function Get_Reservation (T : Transfer) return Integer;

   function Is_Control (T : Transfer) return Boolean;
   function Get_Condition (T : Transfer) return Tagatha_Condition;
   function Get_Destination (T : Transfer)
                            return Tagatha.Labels.Tagatha_Label;

   function Same_Operand (Left, Right : Transfer_Operand) return Boolean;

   function Show (Item : Transfer) return String;
   function Show (Item : Transfer_Operand) return String;

   function Is_Constant (Item : Transfer_Operand) return Boolean;
   function Is_Constant_Zero (Item : Transfer_Operand) return Boolean;
   function Is_Argument (Item : Transfer_Operand) return Boolean;
   function Is_Local    (Item : Transfer_Operand) return Boolean;
   function Is_Result   (Item : Transfer_Operand) return Boolean;

   function Has_Slice (Item : Transfer_Operand) return Boolean;
   function Slice_Fits (Item : Transfer_Operand;
                        Size : Tagatha_Size)
                       return Boolean;
   function Get_Slice_Byte_Offset (Item : Transfer_Operand)
                                  return Tagatha_Integer;
   function Get_Slice_Bit_Offset  (Item : Transfer_Operand)
                                  return Tagatha_Integer;
   function Get_Slice_Byte_Length (Item : Transfer_Operand)
                                  return Tagatha_Integer;
   function Get_Slice_Bit_Length  (Item : Transfer_Operand)
                                  return Tagatha_Integer;

   function Get_Slice_Mask (Item : Transfer_Operand) return Tagatha_Integer;

   function Slice (Item   : Transfer_Operand;
                   Offset : Natural;
                   Size   : Tagatha_Size)
                  return Transfer_Operand;

   function Get_Value (Item : Transfer_Operand)
                      return Tagatha.Constants.Tagatha_Constant;
   function Get_Arg_Offset (Item : Transfer_Operand) return Argument_Offset;
   function Get_Local_Offset (Item : Transfer_Operand) return Local_Offset;

   function Is_Simple    (Item : Transfer) return Boolean;
   function Has_Operator (Item : Transfer) return Boolean;

   function Get_Destination (Item : Transfer) return Transfer_Operand;
   function Get_Source (Item : Transfer) return Transfer_Operand;
   function Get_Source_1 (Item : Transfer) return Transfer_Operand;
   function Get_Source_2 (Item : Transfer) return Transfer_Operand;
   function Get_Operator (Item : Transfer) return Tagatha_Operator;

private

   type Transfer_Operand_Type is
     (T_No_Operand, T_Stack, T_Temporary,
      T_Local, T_Argument, T_Result, T_Immediate,
      T_Condition);

   type Bit_Slice is
      record
         First_Bit : Tagatha_Integer;
         Last_Bit  : Tagatha_Integer;
      end record;

   type Source_Modification is
      record
         Have_Slice : Boolean;
         Have_Size  : Boolean;
         Slice      : Bit_Slice;
         Size       : Tagatha_Size;
      end record;

   No_Modification : constant Source_Modification := (False, False,
                                                      (0, 0),
                                                      Default_Integer_Size);

   type Transfer_Operand (Op : Transfer_Operand_Type := T_No_Operand) is
      record
         Modifiers     : Source_Modification;
         case Op is
            when T_No_Operand =>
               null;
            when T_Stack =>
               null;
            when T_Condition =>
               null;
            when T_Temporary =>
               Temp       : Tagatha.Temporaries.Temporary;
            when T_Local =>
               Loc_Offset : Local_Offset;
            when T_Argument =>
               Arg_Offset : Argument_Offset;
            when T_Result =>
               null;
            when T_Immediate =>
               Value      : Tagatha.Constants.Tagatha_Constant;
         end case;
      end record;

   Null_Operand : constant Transfer_Operand :=
     (T_No_Operand, No_Modification);

   type Array_Of_Operands is array (Positive range <>) of Transfer_Operand;

   type Transfer_Type is
     (T_Control,
      T_Data,
      T_Change_Stack);

   type Transfer is
      record
         Trans       : Transfer_Type;
         Reserve     : Integer;
         Label       : Tagatha.Labels.Tagatha_Label;
         Condition   : Tagatha_Condition;
         Destination : Tagatha.Labels.Tagatha_Label;
         Self        : Boolean;
         Src_1       : Transfer_Operand;
         Src_2       : Transfer_Operand;
         Dst         : Transfer_Operand;
         Op          : Tagatha_Operator;
      end record;

end Tagatha.Transfers;
