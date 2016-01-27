with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash_Case_Insensitive;
with Ada.Strings.Fixed.Equal_Case_Insensitive;

package body Aquarius.Keys.Sequences is

   package Key_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Aquarius_Key,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   Key_Map : Key_Maps.Map;
   Got_Key_Map : Boolean := False;

   procedure Create_Key_Map;

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

   -----------
   -- Clear --
   -----------

   procedure Clear (Sequence : in out Key_Sequence) is
   begin
      Sequence.Length := 0;
   end Clear;

   --------------------
   -- Create_Key_Map --
   --------------------

   procedure Create_Key_Map is

      procedure Key (Name : String;
                     Value : Aquarius_Key);

      ---------
      -- Key --
      ---------

      procedure Key (Name : String;
                     Value : Aquarius_Key)
      is
      begin
         Key_Map.Insert (Name, Value);
      end Key;

   begin
      Key ("down", Down_Arrow);
      Key ("left", Left_Arrow);
      Key ("right", Right_Arrow);
      Key ("up", Up_Arrow);

      for I in Function_Keys loop
         declare
            Name : String := Function_Keys'Image (I - Function_Keys'First);
         begin
            Name (Name'First) := 'f';
            Key (Name, I);
         end;
      end loop;

      Got_Key_Map := True;

   end Create_Key_Map;

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

   ----------
   -- Keys --
   ----------

   function Keys (Sequence : Key_Sequence) return Array_Of_Keys is
   begin
      return Sequence.Keys (1 .. Sequence.Length);
   end Keys;

   --------------------
   -- Parse_Sequence --
   --------------------

   function Parse_Sequence (Text : String) return Key_Sequence is
      Index  : Positive := Text'First;
      Key    : Aquarius_Key;
      Result : Key_Sequence;

      procedure Parse_Key;

      function Special_Key (Name : String) return Aquarius_Key;

      ---------------
      -- Parse_Key --
      ---------------

      procedure Parse_Key is
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
                  Key :=
                    Special_Key (Text (Start .. Index - 1));
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

      -----------------
      -- Special_Key --
      -----------------

      function Special_Key (Name : String) return Aquarius_Key is
      begin
         if Key_Map.Contains (Name) then
            return Key_Map (Name);
         else
            return Null_Key;
         end if;
      end Special_Key;

   begin
      if not Got_Key_Map then
         Create_Key_Map;
      end if;

      while Index <= Text'Length loop
         Parse_Key;
         if Key /= Null_Key then
            Add_Key (Result, Key);
         end if;
      end loop;
      return Result;
   end Parse_Sequence;

end Aquarius.Keys.Sequences;
