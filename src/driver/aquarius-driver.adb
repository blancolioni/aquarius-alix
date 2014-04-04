with Ada.Exceptions;
with Ada.Text_IO;

with Aquarius.Buffers;
with Aquarius.Command_Line;
with Aquarius.Configuration;
with Aquarius.Projects;
with Aquarius.Sections.Welcome;
with Aquarius.Target.Manager;
with Aquarius.Trees.Properties;
with Aquarius.UI.Gtk;

procedure Aquarius.Driver is
begin

   Aquarius.Configuration.Load_Configuration;
   Aquarius.Target.Manager.Set_Target (Aquarius.Command_Line.Target);

   declare
      UI       : constant Aquarius.UI.Aquarius_UI :=
                   Aquarius.UI.Gtk.Create_Gtk_UI;
   begin
      UI.Init;
      if Aquarius.Configuration.Last_Project = "" then
         UI.Show_Section (Aquarius.Sections.Welcome.Create_Welcome_Section,
                          Hint_X => 100,
                          Hint_Y => 0);
      else
         UI.Show_Section (Aquarius.Sections.Welcome.Create_Welcome_Section,
                          Hint_X => 100,
                          Hint_Y => 0);
         declare
            Project_Buffer : constant Aquarius.Buffers.Aquarius_Buffer :=
              Aquarius.Buffers.Load_Buffer_From_File
              (UI, Aquarius.Configuration.Last_Project);
            Project : constant Aquarius.Projects.Aquarius_Project :=
                               Aquarius.Trees.Properties.Get_Project
                                 (Project_Buffer.Program.all);
            Main           : constant Aquarius.Buffers.Aquarius_Buffer :=
                               Project.Get_Main_Buffer;
            pragma Unreferenced (Main);
         begin
            UI.Show_Project (Project);
         end;
      end if;

      UI.Start;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line
        ("exception: " & Ada.Exceptions.Exception_Name (E));
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Message (E));
      Ada.Text_IO.Put ("Press return to exit");
      Ada.Text_IO.Flush;
      Ada.Text_IO.Skip_Line;

end Aquarius.Driver;
