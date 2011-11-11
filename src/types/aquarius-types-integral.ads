with Aquarius.Types.Discrete;
with Aquarius.Values;

package Aquarius.Types.Integral is

   type Root_Integer_Type is
     abstract new Aquarius.Types.Discrete.Root_Discrete_Type with private;

   type Signed_Integer_Type is new Root_Integer_Type with private;
   type Unsigned_Integer_Type is new Root_Integer_Type with private;

   function Universal_Integer_Type return Aquarius_Type;

   function New_Signed_Integer_Type
     (From, To : Aquarius.Values.Aquarius_Value)
     return Aquarius_Type;

   function New_Unsigned_Integer_Type
     (Modulus  : Aquarius.Values.Aquarius_Value)
     return Aquarius_Type;

private

   type Root_Integer_Type is
     abstract new Aquarius.Types.Discrete.Root_Discrete_Type
     with null record;

   overriding
   function Description (Item : Root_Integer_Type)
                 return String;

   type Signed_Integer_Type is new Root_Integer_Type with
      record
         From, To : Aquarius.Values.Aquarius_Value;
      end record;

   overriding
   function Create_Derived_Type (Item : Signed_Integer_Type)
                                return Aquarius_Type;

   type Unsigned_Integer_Type is new Root_Integer_Type with
      record
         Modulus  : Aquarius.Values.Aquarius_Value;
      end record;

   overriding
   function Create_Derived_Type (Item : Unsigned_Integer_Type)
                                return Aquarius_Type;

end Aquarius.Types.Integral;
