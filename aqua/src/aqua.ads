package Aqua is

   type Word is mod 2 ** 16;
   type Byte is mod 2 ** 8;

   type Bit_Index is range 0 .. 15;

   type Address is mod 2 ** 15;
   type External_Reference is mod 2 ** 14;
   type Aqua_Integer is range -2 ** 13 .. 2 ** 13 - 1;
   type String_Reference is mod 2 ** 12;
   type Subroutine_Reference is mod 2 ** 12;

   type Array_Of_Words is array (Positive range <>) of Word;

   Address_Mask_Bits     : constant := 16#8000#;
   Address_Mask_Value    : constant := 16#8000#;

   External_Mask_Bits    : constant := 2#1100_0000_0000_0000#;
   External_Mask_Value   : constant := 2#0100_0000_0000_0000#;

   String_Mask_Bits      : constant := 2#1111_0000_0000_0000#;
   String_Mask_Value     : constant := 2#1110_0000_0000_0000#;

   Integer_Mask_Bits     : constant := 2#1100_0000_0000_0000#;
   Integer_Mask_Value    : constant := 2#0000_0000_0000_0000#;

   function Is_Integer (Value : Word) return Boolean;
   function Get_Integer (Value : Word) return Aqua_Integer;
   function To_Integer_Word (Value : Aqua_Integer) return Word;

   function Is_Address (Value : Word) return Boolean;
   function Get_Address (Value : Word) return Address;
   function To_Address_Word (Addr : Address) return Word;

   function Is_External_Reference (Value : Word) return Boolean;
   function Get_External_Reference (Value : Word) return External_Reference
     with Pre => Is_External_Reference (Value);
   function To_External_Word (Reference : External_Reference)
                              return Word;

   function Is_String_Reference (Value : Word) return Boolean;
   function Get_String_Reference (Value : Word) return String_Reference
     with Pre => Is_String_Reference (Value);
   function To_String_Word (Reference : String_Reference) return Word;

   function Get_Bits
     (Value : Word;
      Start : Bit_Index;
      Count : Bit_Index)
      return Word;

   type Memory_Interface is interface;

   function Get_Byte (Memory : Memory_Interface;
                      Addr   : Address)
                      return Byte
                      is abstract;

   procedure Set_Byte (Memory : in out Memory_Interface;
                       Addr   : Address;
                       Value  : Byte)
   is abstract;

   function Get_Word (Memory : Memory_Interface'Class;
                      Addr   : Address)
                      return Word;

   procedure Set_Word (Memory : in out Memory_Interface'Class;
                       Addr   : Address;
                       Value  : Word);

   type External_Object_Interface is interface;

   function Name (Item : External_Object_Interface) return String
                  is abstract;

   function Text (Item : External_Object_Interface) return String
                  is abstract;

   function Show (Item : External_Object_Interface) return String
                  is abstract;

end Aqua;
