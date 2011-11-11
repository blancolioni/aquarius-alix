package body Aquarius.Types.Private_Types is

   type Private_Type is new Root_Aquarius_Type with
      record
         Full_Type : Aquarius_Type;
      end record;

   overriding
   function Create_Derived_Type (Item : Private_Type)
                                return Aquarius_Type;

   overriding
   function Description (Item : Private_Type)
                        return String;

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   overriding
   function Create_Derived_Type
     (Item : Private_Type)
     return Aquarius_Type
   is
   begin
      return new Private_Type'(Item);
   end Create_Derived_Type;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description (Item : Private_Type)
                        return String
   is
   begin
      if Item.Full_Type /= null then
         return Item.Full_Type.Description;
      else
         return "a private type";
      end if;
   end Description;

   ----------------
   -- Is_Private --
   ----------------

   function Is_Private_Type
     (Item : access Root_Aquarius_Type'Class)
     return Boolean
   is
   begin
      return Item.all in Private_Type'Class;
   end Is_Private_Type;

   ----------------------
   -- New_Private_Type --
   ----------------------

   function New_Private_Type return Aquarius_Type is
   begin
      return new Private_Type;
   end New_Private_Type;

   -------------------
   -- Set_Full_Type --
   -------------------

   procedure Set_Full_Type
     (Item      : access Root_Aquarius_Type'Class;
      Full_Type : access Root_Aquarius_Type'Class)
   is
   begin
      Private_Type (Item.all).Full_Type := Aquarius_Type (Full_Type);
   end Set_Full_Type;

end Aquarius.Types.Private_Types;
