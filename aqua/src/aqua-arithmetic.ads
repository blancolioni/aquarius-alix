package Aqua.Arithmetic is

   --  type-aware arithmetic

   procedure Inc (X : in out Word;
                  Y : in     Positive);

   procedure Dec (X : in out Word;
                  Y : in     Positive);

   function Relative_Address
     (Location : Address;
      Reference : Address)
      return Address;

end Aqua.Arithmetic;
