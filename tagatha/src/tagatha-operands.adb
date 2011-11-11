package body Tagatha.Operands is

   ----------------------
   -- Argument_Operand --
   ----------------------

   function Argument_Operand (Offset : Argument_Offset)
                             return Tagatha_Operand
   is
   begin
      return new Tagatha_Operand_Record'(O_Argument, Offset);
   end Argument_Operand;

   ----------------------
   -- Constant_Operand --
   ----------------------

   function Constant_Operand (Value : Tagatha.Constants.Tagatha_Constant)
                             return Tagatha_Operand
   is
   begin
      return new Tagatha_Operand_Record'(O_Constant, Value);
   end Constant_Operand;

   ----------------------
   -- Constant_Operand --
   ----------------------

   function Constant_Operand (Value : Tagatha_Floating_Point)
                             return Tagatha_Operand
   is
   begin
      return Constant_Operand (Constants.Floating_Point_Constant (Value));
   end Constant_Operand;

   ----------------------
   -- Constant_Operand --
   ----------------------

   function Constant_Operand (Value : Tagatha_Integer)
                             return Tagatha_Operand
   is
   begin
      return Constant_Operand (Constants.Integer_Constant (Value));
   end Constant_Operand;

   --------------------
   -- Get_Arg_Offset --
   --------------------

   function Get_Arg_Offset (Item : Tagatha_Operand) return Argument_Offset is
   begin
      return Item.Arg_Offset;
   end Get_Arg_Offset;

   ----------------------
   -- Get_Local_Offset --
   ----------------------

   function Get_Local_Offset (Item : Tagatha_Operand) return Local_Offset is
   begin
      return Item.Loc_Offset;
   end Get_Local_Offset;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Item : Tagatha_Operand)
                      return Tagatha.Constants.Tagatha_Constant
   is
   begin
      return Item.Value;
   end Get_Value;

   -----------------
   -- Is_Argument --
   -----------------

   function Is_Argument (Item : Tagatha_Operand) return Boolean is
   begin
      return Item.Operand_Type = O_Argument;
   end Is_Argument;

   -----------------
   -- Is_Constant --
   -----------------

   function Is_Constant (Item : Tagatha_Operand) return Boolean is
   begin
      return Item.Operand_Type = O_Constant;
   end Is_Constant;

   --------------
   -- Is_Local --
   --------------

   function Is_Local (Item : Tagatha_Operand) return Boolean is
   begin
      return Item.Operand_Type = O_Local;
   end Is_Local;

   ---------------
   -- Is_Result --
   ---------------

   function Is_Result (Item : Tagatha_Operand) return Boolean is
   begin
      return Item.Operand_Type = O_Result;
   end Is_Result;

   -------------------
   -- Label_Operand --
   -------------------

   function Label_Operand (Label : Tagatha.Labels.Tagatha_Label)
                          return Tagatha_Operand
   is
   begin
      return Constant_Operand (Constants.Label_Constant (Label));
   end Label_Operand;

   --------=----------
   -- Local_Operand --
   -------------------

   function Local_Operand (Offset : Local_Offset)
                          return Tagatha_Operand
   is
   begin
      return new Tagatha_Operand_Record'(O_Local, Offset);
   end Local_Operand;

   ------------------
   -- Null_Operand --
   ------------------

   function Null_Operand return Tagatha_Operand is
   begin
      return null;
   end Null_Operand;

   function Result_Operand return Tagatha_Operand is
   begin
      return new Tagatha_Operand_Record'(Operand_Type => O_Result);
   end Result_Operand;

   ----------
   -- Show --
   ----------

   function Show (Operand : Tagatha_Operand) return String is
   begin
      case Operand.Operand_Type is
         when O_Constant =>
            return Constants.Show (Operand.Value);
         when O_Argument =>
            return "arg" & Argument_Offset'Image (-Operand.Arg_Offset);
         when O_Local =>
            return "loc" & Local_Offset'Image (-Operand.Loc_Offset);
         when O_Result =>
            return "result";
      end case;
   end Show;

end Tagatha.Operands;
