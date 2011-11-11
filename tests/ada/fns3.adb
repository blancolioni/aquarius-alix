procedure fns3 is

   type N is new Integer;

   function f1 (X : Integer) return N is
   begin
      return 1;
   end f1;

   function f1 (X : Integer) return Integer is
   begin
     return 2;
   end f1;

   T  : character;

begin

   T := character'Val (f1 (3));

end;
 
