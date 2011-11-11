with Aquarius.Buffers;
with Aquarius.Command_Line;
with Aquarius.Driver;
with Aquarius.GUI;
with Aquarius.Projects;
;
with Aquarius.Trees.Properties;

procedure Aquarius.Gtk_Main is

begin

   Aquarius.Driver.Start;

   if Aquarius.Driver.Show_UI then

      Aquarius.GUI.Create_Glade_UI;

      if Aquarius.Command_Line.Project_Name /= "" then
         declare
            Project_Buffer : constant Aquarius.Buffers.Aquarius_Buffer :=
              Aquarius.Buffers.Load_Buffer_From_File
              (Aquarius.GUI.Current_UI,
               Aquarius.Command_Line.Project_Name);
            Project : constant Aquarius.Projects.Aquarius_Project :=
              Aquarius.Trees.Properties.Get_Project (Project_Buffer.Program);
         begin
            Aquarius.GUI.Launch_GUI (With_Project => Project);
         end;
      else
         Aquarius.GUI.Launch_GUI
           (With_File =>
              Aquarius.Command_Line.Extra_Arguments);
      end if;

      Aquarius.Trace.End_Trace;

   end if;

end Aquarius.Gtk_Main;
