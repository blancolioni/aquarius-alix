program GCD;

var
   A, B : Integer;
   T    : Integer;

begin

  A := 34;
  B := 12;

  while B <> 0 do
     begin
        T := B;
        B := A mod B;
        A := T
     end

end.

