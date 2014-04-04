with Ada.Text_IO;

with Aquarius.Config_Paths;

package body Aquarius.Sections.Welcome is

   ----------------------------
   -- Create_Welcome_Section --
   ----------------------------

   function Create_Welcome_Section
      return Aquarius.Sections.Aquarius_Section
   is
      Result : constant Aquarius_Section :=
                 new Root_Aquarius_Section;
   begin
      Result.Create ("aquarius:welcome");
      Result.Background :=
        Aquarius.Colours.Parse_Colour ("yellow");

      declare
         use Ada.Text_IO;
         File : File_Type;
      begin
         Open (File, In_File,
               Aquarius.Config_Paths.Config_Path & "/welcome.txt");
         while not End_Of_File (File) loop
            Result.Put_Line (Get_Line (File));
         end loop;
         Close (File);
      end;

      return Result;
   end Create_Welcome_Section;

end Aquarius.Sections.Welcome;
