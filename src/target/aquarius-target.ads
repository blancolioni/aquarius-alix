package Aquarius.Target is

   type Aquarius_Frame_Offset is new Integer;

   type Root_Aquarius_Target is tagged private;

   --  All sizes are in bits

   function Word_Size (Target : access Root_Aquarius_Target)
                      return Positive;

   function Address_Size (Target : access Root_Aquarius_Target)
                         return Positive;

   type Aquarius_Target is access all Root_Aquarius_Target'Class;

   function Current_Target return Aquarius_Target;

   procedure Set_Current_Target (Target : Aquarius_Target);

   function Size_In_Words (Size_In_Bits : Natural) return Natural;

private

   type Root_Aquarius_Target is tagged
      record
         Word_Size    : Positive;
         Address_Size : Positive;
      end record;

end Aquarius.Target;
