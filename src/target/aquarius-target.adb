package body Aquarius.Target is

   Local_Current_Target : Aquarius_Target;

   ------------------
   -- Address_Size --
   ------------------

   function Address_Size (Target : access Root_Aquarius_Target)
                         return Positive
   is
   begin
      return Target.Address_Size;
   end Address_Size;

   --------------------
   -- Current_Target --
   --------------------

   function Current_Target return Aquarius_Target is
   begin
      return Local_Current_Target;
   end Current_Target;

   ------------------------
   -- Set_Current_Target --
   ------------------------

   procedure Set_Current_Target (Target : Aquarius_Target) is
   begin
      Local_Current_Target := Target;
   end Set_Current_Target;

   -------------------
   -- Size_In_Words --
   -------------------

   function Size_In_Words (Size_In_Bits : Natural) return Natural is
   begin
      if Size_In_Bits mod Current_Target.Word_Size = 0 then
         return Size_In_Bits / Current_Target.Word_Size;
      else
         return Size_In_Bits / Current_Target.Word_Size + 1;
      end if;
   end Size_In_Words;

   ---------------
   -- Word_Size --
   ---------------

   function Word_Size (Target : access Root_Aquarius_Target)
                      return Positive
   is
   begin
      return Target.Word_Size;
   end Word_Size;

end Aquarius.Target;
