package Aqua.Words is

   Subroutine_Mask : constant := 2#1111_0000_0000_0000#;
   Subroutine_Tag  : constant := 2#0101_0000_0000_0000#;

   function Is_Subroutine_Reference
     (Value : Word)
      return Boolean;

   function Get_Subroutine_Reference
     (Value : Word)
      return Subroutine_Reference
     with Pre => Is_Subroutine_Reference (Value);

   function To_Subroutine_Word
     (Reference : Subroutine_Reference)
      return Word;

end Aqua.Words;
