procedure With_Composite is

   type R is tagged null record;
   type S is new R with record X : Integer; end record;

   I_S : constant S := (R with X => 12);

begin
   null;
end With_Composite;

