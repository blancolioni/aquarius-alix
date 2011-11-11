with Tagatha.Constants;
with Tagatha.Labels;

package Tagatha.Operands is

   type Tagatha_Operand is private;

   function Null_Operand return Tagatha_Operand;

   function Constant_Operand (Value : Tagatha.Constants.Tagatha_Constant)
                              return Tagatha_Operand;

   function Constant_Operand (Value : Tagatha_Floating_Point)
                              return Tagatha_Operand;

   function Constant_Operand (Value : Tagatha_Integer)
                              return Tagatha_Operand;

   function Label_Operand (Label : Tagatha.Labels.Tagatha_Label)
                          return Tagatha_Operand;

   function Argument_Operand (Offset : Argument_Offset)
                                  return Tagatha_Operand;

   function Local_Operand (Offset : Local_Offset)
                               return Tagatha_Operand;

   function Result_Operand return Tagatha_Operand;

   function Is_Constant (Item : Tagatha_Operand) return Boolean;
   function Is_Argument (Item : Tagatha_Operand) return Boolean;
   function Is_Local    (Item : Tagatha_Operand) return Boolean;
   function Is_Result   (Item : Tagatha_Operand) return Boolean;

   function Get_Value (Item : Tagatha_Operand)
                      return Tagatha.Constants.Tagatha_Constant;
   function Get_Arg_Offset (Item : Tagatha_Operand) return Argument_Offset;
   function Get_Local_Offset (Item : Tagatha_Operand) return Local_Offset;

   function Show (Operand : Tagatha_Operand) return String;

private

   type Tagatha_Operand_Type is
     (O_Constant,
      O_Argument,
      O_Local,
      O_Result);

   type Tagatha_Operand_Record (Operand_Type : Tagatha_Operand_Type) is
      record
         case Operand_Type is
            when O_Constant =>
               Value   : Tagatha.Constants.Tagatha_Constant;
            when O_Argument =>
               Arg_Offset : Argument_Offset;
            when O_Local =>
               Loc_Offset : Local_Offset;
            when O_Result =>
               null;
         end case;
      end record;

   type Tagatha_Operand is access Tagatha_Operand_Record;

end Tagatha.Operands;
