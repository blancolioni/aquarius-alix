with Gtkada.File_Selection;

with Aquarius.GUI.Source;

package body Aquarius.GUI.Files is

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File is
      Name : constant String :=
        Gtkada.File_Selection.File_Selection_Dialog ("Open file",
                                                     Must_Exist => True);
   begin
      if Name /= "" then
         Aquarius.GUI.Source.Load_File (Name);
      end if;
   end Open_File;

end Aquarius.GUI.Files;
