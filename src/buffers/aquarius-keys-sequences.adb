package body Aquarius.Keys.Sequences is

   -------------
   -- Add_Key --
   -------------

   procedure Add_Key (Sequence : in out Key_Sequence;
                      Key      : in     Aquarius_Key)
   is
   begin
      Sequence.Length := Sequence.Length + 1;
      Sequence.Keys (Sequence.Length) := Key;
   end Add_Key;

   ---------------------
   -- Create_Sequence --
   ---------------------

   function Create_Sequence (Keys : Array_Of_Keys) return Key_Sequence is
      Result : Key_Sequence;
   begin
      for I in Keys'Range loop
         Add_Key (Result, Keys (I));
      end loop;
      return Result;
   end Create_Sequence;

   ---------------------
   -- Create_Sequence --
   ---------------------

   function Create_Sequence (Key : Aquarius_Key) return Key_Sequence is
   begin
      return Create_Sequence ((1 => Key));
   end Create_Sequence;

   --------------------
   -- Parse_Sequence --
   --------------------

   function Parse_Sequence (Text : String) return Key_Sequence is
      Index  : Positive := Text'First;
      Key    : Aquarius_Key;
      Result : Key_Sequence;

      procedure Parse_Key (Index : Positive);
      function Special_Key (Name : String) return Aquarius_Key;

      procedure Parse_Key (Index : Positive) is
         Start : Positive;
      begin
         if Text (Index) = '[' then
            Index := Index + 1;
            Start := Index;
            while Index <= Text'Last and then Text (Index) /= ']' loop
               Index := Index + 1;
            end loop;
            if Index > Text'Last then
               Key := Null_Key;
            else
               Key := Special_Key (Text (Start .. Index - 1));
               Index := Index + 1;
            end if;
         elsif Index < Text'Last - 1 and then
           Text (Index) = 'C' and then
           Text (Index + 1) = '-'
         then
            Key := Modify (Character_Key (Text (Index + 2)),
                           Control => True);
            Index := Index + 3;
         else
            Key := Character_Key (Text (Index));
            Index := Index + 1;
         end if;
      end Parse_Key;

   begin
      while Index <= Text'Length loop
         Parse_Key (Index);
         if Key /= Null_Key then
            Add_Key (Result, Key);
         end if;
      end loop;
      return Result;
   end Parse_Sequence;

end Aquarius.Keys.Sequences;
