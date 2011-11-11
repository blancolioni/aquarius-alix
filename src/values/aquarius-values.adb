package body Aquarius.Values is

   function Get_Bits (N : Natural) return Natural;
   --  return number of significant bits in N

   --------------
   -- Get_Bits --
   --------------

   function Get_Bits (N : Natural) return Natural is
      It     : Natural := N;
      Result : Natural := 0;
   begin
      while It > 0 loop
         It := It / 2;
         Result := Result + 1;
      end loop;
      return Result;
   end Get_Bits;

   ----------------
   -- Range_Bits --
   ----------------

   function Range_Bits (From, To : Aquarius_Value) return Natural is
      Neg_Bits, Pos_Bits : Natural := 0;
   begin
      if From.Size /= Small or else To.Size /= Small then
         raise Constraint_Error with "expected two integers in Range_Bits";
      end if;

      if From.Small_Value >= To.Small_Value then
         return 0;
      else
         if From.Small_Value < 0 then
            Neg_Bits := Get_Bits (-(From.Small_Value + 1));
         end if;
         if To.Small_Value >= 0 then
            Pos_Bits := Get_Bits (To.Small_Value);
         end if;
         if Pos_Bits > 0 and then Neg_Bits > 0 then
            return 1 + Natural'Max (Pos_Bits, Neg_Bits);
         else
            return Natural'Max (Pos_Bits, Neg_Bits);
         end if;
      end if;

   end Range_Bits;

   --------------
   -- To_Value --
   --------------

   function To_Value (Item : Integer) return Aquarius_Value is
   begin
      return (Small, Item);
   end To_Value;

   --------------
   -- To_Value --
   --------------

   function To_Value (Item : String)  return Aquarius_Value is
      Storage : System.Storage_Elements.Storage_Array (1 .. Item'Length);
   begin
      for I in Storage'Range loop
         Storage (I) := Character'Pos (Item (Positive (I)));
      end loop;
      return (Large, new System.Storage_Elements.Storage_Array'(Storage));
   end To_Value;

end Aquarius.Values;
