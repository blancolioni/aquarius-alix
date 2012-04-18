procedure Raise_With (X : Integer) is

   subtype T is Integer range 1 .. 10;

begin

   if X in T then
      raise Constraint_Error with "X was in T";
   end if;

end Raise_With;
