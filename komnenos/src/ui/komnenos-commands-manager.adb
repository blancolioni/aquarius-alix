package body Komnenos.Commands.Manager is

   --------------
   -- Can_Redo --
   --------------

   function Can_Redo (Manager : Command_Manager'Class) return Boolean is
   begin
      return Manager.Current <= Manager.Commands.Last_Index;
   end Can_Redo;

   --------------
   -- Can_Undo --
   --------------

   function Can_Undo (Manager : Command_Manager'Class) return Boolean is
   begin
      return Manager.Current > 1;
   end Can_Undo;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Manager : in out Command_Manager'Class;
      Command : in out Root_Komnenos_Command'Class;
      Entity  : Komnenos.Entities.Entity_Reference)
   is
   begin
      Command.Entity := Entity;
      Command.Execute;
      Manager.Commands.Set_Length
        (Ada.Containers.Count_Type (Manager.Current - 1));
      Manager.Commands.Append (Command);
      Manager.Current := Manager.Current + 1;
   end Execute;

   ----------
   -- Redo --
   ----------

   procedure Redo (Manager : in out Command_Manager'Class) is
   begin
      Manager.Commands (Manager.Current).Execute;
      Manager.Current := Manager.Current + 1;
   end Redo;

   ----------
   -- Undo --
   ----------

   procedure Undo (Manager : in out Command_Manager'Class) is
   begin
      Manager.Current := Manager.Current - 1;
      Manager.Commands (Manager.Current).Undo;
   end Undo;

end Komnenos.Commands.Manager;
