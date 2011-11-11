with Aquarius.Interaction;
with Aquarius.Projects;
with Aquarius.UI;

package Aquarius.GUI is

   procedure Launch_GUI
     (With_File    : String := "";
      With_Project : Aquarius.Projects.Aquarius_Project := null);

   function Current_UI
     return access Aquarius.UI.Aquarius_UI'Class;

private

   function Current_Project
     return Aquarius.Projects.Aquarius_Project;

   type Gtk_UI is
     new Aquarius.UI.Aquarius_UI
   with null record;

   overriding
   function Name (Item : Gtk_UI) return String;

   overriding
   procedure Update_Message_View (Item : in out Gtk_UI);

   overriding
   procedure Show_Interactor
     (UI    : access Gtk_UI;
      Item  : access Aquarius.Interaction.Interactor'Class);

end Aquarius.GUI;
