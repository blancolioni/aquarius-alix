with Ada.Command_Line;
with Ada.Directories;

with As.Assembler;

procedure As.Driver is
   Asm : constant As.Assembler.Reference := As.Assembler.New_Assembler;
begin
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         Path : constant String := Ada.Command_Line.Argument (I);
      begin
         if Ada.Directories.Exists (Path) then
            Asm.Load (Path);
         end if;
      end;
   end loop;

   Asm.Assemble;
   Asm.List (Ada.Directories.Base_Name (Ada.Command_Line.Argument (1))
             & ".lst");
   Asm.Write (Ada.Directories.Base_Name (Ada.Command_Line.Argument (1))
              & ".aqo");

end As.Driver;
