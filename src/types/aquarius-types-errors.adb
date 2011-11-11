package body Aquarius.Types.Errors is

   type Error_Type is new Root_Aquarius_Type with null record;

   overriding
   function Description (Item : Error_Type) return String;
   overriding
   function Create_Derived_Type
     (Item : Error_Type)
     return Aquarius_Type;

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   overriding
   function Create_Derived_Type
     (Item : Error_Type)
     return Aquarius_Type
   is
   begin
      return new Error_Type'(Item);
   end Create_Derived_Type;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description (Item : Error_Type) return String is
      pragma Unreferenced (Item);
   begin
      return "a type";
   end Description;

   -------------------
   -- Is_Error_Type --
   -------------------

   function Is_Error_Type (Item : Aquarius_Type) return Boolean is
   begin
      return Item.all in Error_Type'Class;
   end Is_Error_Type;

   --------------------
   -- New_Error_Type --
   --------------------

   function New_Error_Type return Aquarius_Type is
   begin
      return new Error_Type;
   end New_Error_Type;

end Aquarius.Types.Errors;
