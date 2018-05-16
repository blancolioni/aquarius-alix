with Aqua.Drivers;

package Aquarius.Programs.Aqua_Driver is

   function Aquarius_Tree_Driver
     return Aqua.Drivers.Aqua_Driver;

private

   type Aquarius_Tree_Driver_Record is
     new Aqua.Drivers.Root_Aqua_Driver with
      record
         Current : Program_Tree;
      end record;

   overriding procedure Update
     (Driver : in out Aquarius_Tree_Driver_Record);

   procedure Write_String
     (Driver : in out Aquarius_Tree_Driver_Record'Class;
      S      : String);

   function Read_String
     (Driver : Aquarius_Tree_Driver_Record'Class)
      return String;

end Aquarius.Programs.Aqua_Driver;
