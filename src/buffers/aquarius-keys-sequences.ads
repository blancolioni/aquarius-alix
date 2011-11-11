package Aquarius.Keys.Sequences is

   type Key_Sequence is private;

   type Array_Of_Keys is array (Positive range <>) of Aquarius_Key;

   function Create_Sequence (Keys : Array_Of_Keys) return Key_Sequence;
   function Create_Sequence (Key : Aquarius_Key) return Key_Sequence;
   procedure Add_Key (Sequence : in out Key_Sequence;
                      Key      : in     Aquarius_Key);

   function Parse_Sequence (Text : String) return Key_Sequence;

private

   Max_Sequence_Length : constant := 4;

   type Key_Sequence_Count is range 0 .. Max_Sequence_Length;
   subtype Key_Sequence_Index is
     Key_Sequence_Count range 1 .. Key_Sequence_Count'Last;

   type Key_Sequence_Array is array (Key_Sequence_Index) of Aquarius_Key;

   type Key_Sequence is
      record
         Length : Key_Sequence_Count := 0;
         Keys   : Key_Sequence_Array := (others => 0);
      end record;

end Aquarius.Keys.Sequences;
