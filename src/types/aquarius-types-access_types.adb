with Aquarius.Target;

package body Aquarius.Types.Access_Types is

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   overriding
   function Create_Derived_Type
     (Item : Access_Type) return Aquarius_Type
   is
   begin
      return new Access_Type'(Item);
   end Create_Derived_Type;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description
     (Item : Access_Type) return String
   is
      pragma Unreferenced (Item);
   begin
      return "an access type";
   end Description;

   -------------------
   -- Get_Access_To --
   -------------------

   function Get_Access_To (Item : Aquarius_Type) return Aquarius_Type is
   begin
      return Access_Type (Item.all).Access_To;
   end Get_Access_To;

   -------------------
   -- Is_Access_All --
   -------------------

   function Is_Access_All   (Item : Aquarius_Type) return Boolean is
   begin
      return Access_Type (Item.all).Access_All;
   end Is_Access_All;

   -------------------
   -- Is_Not_Null --
   -------------------

   function Is_Not_Null   (Item : Aquarius_Type) return Boolean is
   begin
      return Access_Type (Item.all).Not_Null;
   end Is_Not_Null;

   ---------------------
   -- New_Access_Type --
   ---------------------

   function New_Access_Type (Access_To   : Aquarius_Type;
                             Access_All  : Boolean;
                             Not_Null    : Boolean)
                            return Aquarius_Type
   is
      Result : aliased Access_Type;
   begin
      Result.Set_Default_Size
        (Aquarius.Target.Address_Size (Aquarius.Target.Current_Target));
      Result.Access_To := Access_To;
      Result.Access_All := Access_All;
      Result.Not_Null   := Not_Null;
      return new Access_Type'(Result);
   end New_Access_Type;

   --------------------
   -- Set_Access_All --
   --------------------

   procedure Set_Access_All (Item       : Aquarius_Type;
                             Access_All : Boolean)
   is
   begin
      Access_Type (Item.all).Access_All := Access_All;
   end Set_Access_All;

   -------------------
   -- Set_Access_To --
   -------------------

   procedure Set_Access_To (Item      : Aquarius_Type;
                            Access_To : Aquarius_Type)
   is
   begin
      Access_Type (Item.all).Access_To := Access_To;
   end Set_Access_To;

   ------------------
   -- Set_Not_Null --
   ------------------

   procedure Set_Not_Null (Item       : Aquarius_Type;
                           Not_Null : Boolean)
   is
   begin
      Access_Type (Item.all).Not_Null := Not_Null;
   end Set_Not_Null;

end Aquarius.Types.Access_Types;
