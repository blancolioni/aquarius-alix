package Aquarius.Keys.Sequences is

   type Key_Sequence is private;

   type Array_Of_Keys is array (Positive range <>) of Aquarius_Key;

   function Create_Sequence (Keys : Array_Of_Keys) return Key_Sequence;
   function Create_Sequence (Key : Aquarius_Key) return Key_Sequence;
   procedure Clear (Sequence : in out Key_Sequence);
   procedure Add_Key (Sequence : in out Key_Sequence;
                      Key      : in     Aquarius_Key);

   function Parse_Sequence (Text : String) return Key_Sequence;
   function Keys (Sequence : Key_Sequence) return Array_Of_Keys;

private

   Max_Sequence_Length : constant := 8;

   type Key_Sequence is
      record
         Length : Natural := 0;
         Keys   : Array_Of_Keys (1 .. Max_Sequence_Length);
      end record;

end Aquarius.Keys.Sequences;
