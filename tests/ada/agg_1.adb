procedure Agg_1 is
   type R_1 is record X, Y : Integer; end record;
   type R_2 is record X : Integer; end record;
   V_1 : R_1;
   V_2 : R_2;
begin
   V_1 := (1, 2);
   V_2 := (X => 1);
   V_1 := (X => 1, Y => 2);
   V_1 := (1, Y => 2);
   V_1 := 1;   --  error
end Agg_1;



