with Aquarius.Buffers;
with Aquarius.Command_Line;
with Aquarius.Projects;
with Aquarius.Trees.Properties;

with Aquarius.GUI;

package body Aquarius.Editor is

   ------------------
   -- Start_Editor --
   ------------------

   procedure Start_Editor
     (UI : not null access Aquarius.UI.Aquarius_UI'Class)
   is
   begin
      if Aquarius.Command_Line.Project_Name /= "" then
         declare
            Project_Buffer : constant Aquarius.Buffers.Aquarius_Buffer :=
              Aquarius.Buffers.Load_Buffer_From_File
              (UI, Aquarius.Command_Line.Project_Name);
            Project : constant Aquarius.Projects.Aquarius_Project :=
                               Aquarius.Trees.Properties.Get_Project
                                 (Project_Buffer.Program.all);
         begin
            Aquarius.GUI.Launch_GUI (With_Project => Project);
         end;
      else
         Aquarius.GUI.Launch_GUI (With_File =>
                                    Aquarius.Command_Line.Extra_Arguments);
      end if;
   end Start_Editor;

end Aquarius.Editor;
