with System.Storage_Elements;

package Aquarius.Values is

   type Value_Size is (None, Small, Large);

   type Aquarius_Value (Size : Value_Size := Small) is private;

   function To_Value (Item : Integer) return Aquarius_Value;
   function To_Value (Item : String)  return Aquarius_Value;

   No_Value : constant Aquarius_Value;

   function Range_Bits (From, To : Aquarius_Value) return Natural;
   --  Calculate the number of bits required to represent the given range

private

   type Large_Value_Access is
     access System.Storage_Elements.Storage_Array;

   type Aquarius_Value (Size : Value_Size := Small) is
      record
         case Size is
            when None =>
               null;
            when Small =>
               Small_Value : Integer;
            when Large =>
               Large_Value : Large_Value_Access;
         end case;
      end record;

   No_Value : constant Aquarius_Value := (Size => None);

end Aquarius.Values;
