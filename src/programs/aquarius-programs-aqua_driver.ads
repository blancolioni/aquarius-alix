with Aqua.Drivers;

package Aquarius.Programs.Aqua_Driver is

   function To_Address (Tree : Program_Tree) return Aqua.Address;

   function Aquarius_Tree_Driver
     return Aqua.Drivers.Aqua_Driver;

private

   type Aquarius_Tree_Driver_Record is
     new Aqua.Drivers.Root_Aqua_Driver with
      record
         Current : Program_Tree;
      end record;

   overriding function Identity
     (Driver : Aquarius_Tree_Driver_Record)
      return String
   is ("aquarius-program-tree-driver");

   overriding procedure Update
     (Driver : in out Aquarius_Tree_Driver_Record);

   overriding function Monitored
     (Driver : Aquarius_Tree_Driver_Record;
      Register : Aqua.Drivers.Driver_Register_Range)
      return Boolean
   is (Register in 4 .. 7);

   procedure Write_String
     (Driver : in out Aquarius_Tree_Driver_Record'Class;
      S      : String);

   function Read_String
     (Driver : Aquarius_Tree_Driver_Record'Class)
      return String;

   function To_Address (Tree : Program_Tree) return Aqua.Address
   is (Aqua.Address (Tree.Sequence));

end Aquarius.Programs.Aqua_Driver;
