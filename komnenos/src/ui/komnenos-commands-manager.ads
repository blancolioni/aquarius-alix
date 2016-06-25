private with Ada.Containers.Indefinite_Vectors;

package Komnenos.Commands.Manager is

   type Command_Manager is tagged private;

   procedure Execute
     (Manager : in out Command_Manager'Class;
      Command : in out Root_Komnenos_Command'Class;
      Entity  : Komnenos.Entities.Entity_Reference);

   procedure Undo (Manager : in out Command_Manager'Class);
   procedure Redo (Manager : in out Command_Manager'Class);

   function Can_Undo (Manager : Command_Manager'Class) return Boolean;
   function Can_Redo (Manager : Command_Manager'Class) return Boolean;

private

   package Command_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive, Root_Komnenos_Command'Class);

   type Command_Manager is tagged
      record
         Current  : Positive := 1;
         Commands : Command_Vectors.Vector;
      end record;

end Komnenos.Commands.Manager;
