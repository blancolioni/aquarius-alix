package XAquarius.Tasks is

   type Root_Task_Type is abstract tagged private;

   procedure Execute (Item : Root_Task_Type) is abstract;

   procedure Add_Task
     (Item : Root_Task_Type'Class);

   procedure Stop;

   procedure Set_Changed (Name : String);
   procedure Clear_Changed (Name : String);
   function Changed (Name : String) return Boolean;

private

   type Root_Task_Type is abstract tagged null record;

end Aquarius.Tasks;
