package body Aquarius.Types.Integral is

   type Universal_Integer is new Root_Integer_Type with null record;

   overriding
   function Description (Item : Universal_Integer)
                 return String;

   overriding
   function Create_Derived_Type
     (Item : Universal_Integer)
     return Aquarius_Type;

   overriding
   function Unify (Item      : not null access Universal_Integer;
                   With_Type : not null access Root_Aquarius_Type'Class)
                  return Aquarius_Type;

   Local_Universal_Integer : Aquarius_Type;

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   overriding
   function Create_Derived_Type
     (Item : Universal_Integer)
     return Aquarius_Type
   is
   begin
      return new Universal_Integer'(Item);
   end Create_Derived_Type;

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   overriding
   function Create_Derived_Type
     (Item : Unsigned_Integer_Type)
     return Aquarius_Type
   is
   begin
      return new Unsigned_Integer_Type'(Item);
   end Create_Derived_Type;

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   overriding
   function Create_Derived_Type
     (Item : Signed_Integer_Type)
     return Aquarius_Type
   is
   begin
      return new Signed_Integer_Type'(Item);
   end Create_Derived_Type;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description (Item : Universal_Integer)
                 return String
   is
      pragma Unreferenced (Item);
   begin
      return "universal integer";
   end Description;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description (Item : Root_Integer_Type)
                 return String
   is
      pragma Unreferenced (Item);
   begin
      return "an integer type";
   end Description;

   -----------------------------
   -- New_Signed_Integer_Type --
   -----------------------------

   function New_Signed_Integer_Type
     (From, To : Aquarius.Values.Aquarius_Value)
     return Aquarius_Type
   is
      Result : aliased Signed_Integer_Type;
   begin
      Result.From := From;
      Result.To   := To;
      Result.Set_Default_Size (Aquarius.Values.Range_Bits (From, To));

      return new Signed_Integer_Type'(Result);
   end New_Signed_Integer_Type;

   -----------------------------
   -- New_Unsigned_Integer_Type --
   -----------------------------

   function New_Unsigned_Integer_Type
     (Modulus : Aquarius.Values.Aquarius_Value)
     return Aquarius_Type
   is
      Result : Unsigned_Integer_Type;
   begin
      Result.Modulus := Modulus;
      return new Unsigned_Integer_Type'(Result);
   end New_Unsigned_Integer_Type;

   -----------
   -- Unify --
   -----------

   overriding
   function Unify (Item      : not null access Universal_Integer;
                   With_Type : not null access Root_Aquarius_Type'Class)
                  return Aquarius_Type
   is
   begin
      if With_Type.all in Root_Integer_Type'Class then
         return Aquarius_Type (With_Type);
      elsif Is_Named_Type (With_Type) then
         return Unify (Item, Get_Named_Type_Base (With_Type));
      else
         return null;
      end if;
   end Unify;

   ----------------------------
   -- Universal_Integer_Type --
   ----------------------------

   function Universal_Integer_Type return Aquarius_Type is
   begin
      if Local_Universal_Integer = null then
         Local_Universal_Integer := new Universal_Integer;
         Local_Universal_Integer.Set_Universal;
      end if;

      return Local_Universal_Integer;
   end Universal_Integer_Type;

end Aquarius.Types.Integral;
