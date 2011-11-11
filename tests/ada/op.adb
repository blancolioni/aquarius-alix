package body Op is

   function "=" (X : Integer;
                 Y : Boolean)
                return Boolean
   is
   begin
      return True;
   end "=";

end Op;
