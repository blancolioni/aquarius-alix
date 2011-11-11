package body Aquarius.Types.Subtypes is

   type Sub_Type is new Root_Aquarius_Type with
      record
         Base_Type   : Aquarius_Type;
         Constraints : Boolean;
         Low, High   : Aquarius.Values.Aquarius_Value;
      end record;

   overriding
   function Description
     (Item : Sub_Type) return String;

   overriding
   function Create_Derived_Type
     (Item : Sub_Type) return Aquarius_Type;

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   overriding
   function Create_Derived_Type
     (Item : Sub_Type) return Aquarius_Type
   is
   begin
      return new Sub_Type'(Item);
   end Create_Derived_Type;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description
     (Item : Sub_Type) return String
   is
   begin
      return Item.Base_Type.Description;
   end Description;

   -----------------
   -- New_Subtype --
   -----------------

   function New_Subtype
     (From_Type   : access Aquarius.Types.Root_Aquarius_Type'Class)
     return Aquarius_Type
   is
   begin
      return new Sub_Type'(Root_Aquarius_Type with
                             Aquarius_Type (From_Type), False,
                             Aquarius.Values.No_Value,
                             Aquarius.Values.No_Value);
   end New_Subtype;

   -----------------
   -- New_Subtype --
   -----------------

   function New_Subtype
     (From_Type   : access Aquarius.Types.Root_Aquarius_Type'Class;
      Low, High   : in     Aquarius.Values.Aquarius_Value)
     return Aquarius_Type
   is
   begin
      return new Sub_Type'(Root_Aquarius_Type with
                             Aquarius_Type (From_Type), False, Low, High);
   end New_Subtype;

end Aquarius.Types.Subtypes;
