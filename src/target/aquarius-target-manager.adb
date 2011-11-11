with Aquarius.Target.M6510;
with Aquarius.Target.Pdp11;

package body Aquarius.Target.Manager is

   ----------------
   -- Set_Target --
   ----------------

   procedure Set_Target (Name : String) is
      Target : Aquarius_Target;
   begin
      if Name = "6510" then
         Target := M6510.Target_M6510;
      elsif Name = "pdp11" then
         Target := Pdp11.Target_Pdp11;
      else
         Target := M6510.Target_M6510;
      end if;
      Set_Current_Target (Target);
   end Set_Target;

end Aquarius.Target.Manager;
