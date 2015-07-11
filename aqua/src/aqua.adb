package body Aqua is

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address (Value : Word) return Address is
   begin
      return Address (Value and not Address_Mask_Bits);
   end Get_Address;

   --------------
   -- Get_Bits --
   --------------

   function Get_Bits
     (Value : Word;
      Start : Bit_Index;
      Count : Bit_Index)
      return Word
   is
      Result : Word := Value;
   begin
      Result := Result and (2 ** Natural (Start + 1) - 1);
      Result := Result / 2 ** Natural (Start + 1 - Count);
      return Result;
   end Get_Bits;

   ----------------------------
   -- Get_External_Reference --
   ----------------------------

   function Get_External_Reference
     (Value : Word)
      return External_Reference
   is
   begin
      return External_Reference (Value and not External_Mask_Bits);
   end Get_External_Reference;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer (Value : Word) return Aqua_Integer is
   begin
      if (Value and 16#0800#) = 0 then
         return Aqua_Integer (Value);
      elsif Value = 16#0800# then
         return Aqua_Integer'First;
      else
         return -Aqua_Integer (16#1000# - Value);
      end if;
   end Get_Integer;

   --------------------------
   -- Get_String_Reference --
   --------------------------

   function Get_String_Reference
     (Value : Word)
      return String_Reference
   is
   begin
      return String_Reference (Value and not String_Mask_Bits);
   end Get_String_Reference;

   --------------
   -- Get_Word --
   --------------

   function Get_Word
     (Memory : Memory_Interface'Class;
      Addr   : Address)
      return Word
   is
   begin
      return Word (Memory.Get_Byte (Addr))
        + Word (Memory.Get_Byte (Addr + 1)) * 256;
   end Get_Word;

   ----------------
   -- Is_Address --
   ----------------

   function Is_Address (Value : Word) return Boolean is
   begin
      return (Value and Address_Mask_Bits) = Address_Mask_Value;
   end Is_Address;

   ---------------------------
   -- Is_External_Reference --
   ---------------------------

   function Is_External_Reference (Value : Word) return Boolean is
   begin
      return (Value and External_Mask_Bits) = External_Mask_Value;
   end Is_External_Reference;

   ----------------
   -- Is_Integer --
   ----------------

   function Is_Integer (Value : Word) return Boolean is
   begin
      return (Value and Integer_Mask_Bits) = Integer_Mask_Value;
   end Is_Integer;

   --------------
   -- Set_Word --
   --------------

   procedure Set_Word
     (Memory : in out Memory_Interface'Class;
      Addr   : Address;
      Value  : Word)
   is
   begin
      Memory.Set_Byte (Addr, Byte (Value mod 256));
      Memory.Set_Byte (Addr + 1, Byte (Value / 256));
   end Set_Word;

   ---------------------
   -- To_Address_Word --
   ---------------------

   function To_Address_Word (Addr : Address) return Word is
   begin
      return Word (Addr) + Address_Mask_Value;
   end To_Address_Word;

   ----------------------
   -- To_External_Word --
   ----------------------

   function To_External_Word
     (Reference : External_Reference)
      return Word
   is
   begin
      return Word (Reference) + External_Mask_Value;
   end To_External_Word;

   ---------------------
   -- To_Integer_Word --
   ---------------------

   function To_Integer_Word (Value : Aqua_Integer) return Word is
   begin
      if Value >= 0 then
         return Word (Value);
      else
         return 16#1000# - Word (abs Value);
      end if;
   end To_Integer_Word;

   --------------------
   -- To_String_Word --
   --------------------

   function To_String_Word
     (Reference : String_Reference)
      return Word
   is
   begin
      return Word (Reference) + String_Mask_Value;
   end To_String_Word;

end Aqua;
