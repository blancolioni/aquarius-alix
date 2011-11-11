procedure fns2 is

   function f1 (X : Integer) return Character is
   begin
     return Character'Val (X);
   end f1;

   function f1 (X : Integer) return Float is
   begin
      return Float (X);
   end f1;

   T  : Character;

begin

   T := f1 ('s');

end fns2;
 
