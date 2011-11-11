procedure Exceptions is
   X : Natural := 0;
begin
   X := X - 1;
exception
   when Constraint_Error =>
      X := 0;
end Exceptions;
