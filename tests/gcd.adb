---------
-- Gcd --
---------

function Gcd (X, Y : Integer) return Integer is

  A, B : Integer;
  T : Integer;

begin

  A := X;
  B := Y;

  while B /= 0 loop
    T := B;
    B := A mod B;
    A := T;
  end loop;

  return A;

end Gcd;

