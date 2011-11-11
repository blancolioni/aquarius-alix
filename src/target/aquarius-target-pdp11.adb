package body Aquarius.Target.Pdp11 is

   ------------------
   -- Target_Pdp11 --
   ------------------

   function Target_Pdp11 return Aquarius_Target is
   begin
      return new Root_Aquarius_Target'(16, 16);
   end Target_Pdp11;

end Aquarius.Target.Pdp11;
