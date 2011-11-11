package body Aquarius.Target.M6510 is

   ------------------
   -- Target_M6510 --
   ------------------

   function Target_M6510 return Aquarius_Target is
   begin
      --  we can probably stand to configure a 16/16 version as well,
      --  but that makes simple things slower
      return new Root_Aquarius_Target'(8, 16);
   end Target_M6510;

end Aquarius.Target.M6510;
