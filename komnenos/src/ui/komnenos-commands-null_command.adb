package body Komnenos.Commands.Null_Command is

   type Root_Null_Command is
     new Root_Komnenos_Command with null record;

   overriding procedure Execute (Command : in out Root_Null_Command)
   is null;

   overriding procedure Undo (Command : in out Root_Null_Command)
   is null;

   overriding function Show (Command : Root_Null_Command) return String
   is ("[]");

   ------------------
   -- Null_Command --
   ------------------

   function Null_Command return Root_Komnenos_Command'Class is
   begin
      return Result : constant Root_Null_Command :=
        (Root_Komnenos_Command with null record)
      do
         null;
      end return;
   end Null_Command;

end Komnenos.Commands.Null_Command;
