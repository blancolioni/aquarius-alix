procedure fns is

   function f1 (X : Integer) return Character is
   begin
     return Character'Val (X);
   end f1;

   function f1 (X : Integer) return Float is
   begin
      return Float (X);
   end f1;

   T  : Integer;

begin

   T := f1 (2);

end fns;
 
