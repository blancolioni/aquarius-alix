package Aquarius.Tasks is

   type Root_Task_Type is abstract tagged private;

   procedure Execute (Item : Root_Task_Type) is abstract;

   procedure Add_Task
     (Item : Root_Task_Type'Class);

   procedure Stop;

private

   type Root_Task_Type is abstract tagged null record;

end Aquarius.Tasks;
