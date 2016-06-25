package body Komnenos.Commands is

   ----------------
   -- Set_Entity --
   ----------------

   procedure Set_Entity
     (Command : in out Root_Komnenos_Command'Class;
      Entity  : Komnenos.Entities.Entity_Reference)
   is
   begin
      Command.Entity := Entity;
   end Set_Entity;

end Komnenos.Commands;
