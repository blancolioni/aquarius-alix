private with Gtk.Drawing_Area;
private with Gtk.Fixed;
private with Gtk.GEntry;
private with Gtk.Tree_View;
private with Aquarius.References;
private with Aquarius.UI.Gtk_Sections;

with Aquarius.Projects;

package Aquarius.UI.Gtk_UI is

   type Gtk_UI is
     new Root_UI_Type with private;

   overriding
   procedure Init (With_UI : not null access Gtk_UI);

   overriding
   procedure Start (With_UI : in out Gtk_UI);

   overriding
   procedure Show_Section (On      : in out Gtk_UI;
                           Section : Aquarius.Sections.Aquarius_Section;
                           Hint_X  : Integer;
                           Hint_Y  : Integer);

   overriding
   procedure Show_Project
     (User_Interface : in out Gtk_UI;
      Project        : not null access
        Aquarius.Projects.Aquarius_Project_Type'Class);

private

   Max_Refs : constant := 1_000;

   type Gtk_UI is
     new Root_UI_Type with
      record
         Overview    : Gtk.Drawing_Area.Gtk_Drawing_Area;
         Sections    : Gtk.Fixed.Gtk_Fixed;
         Identifiers : Gtk.Tree_View.Gtk_Tree_View;
         Search      : Gtk.GEntry.Gtk_Entry;
         Start_X     : Integer;
         Start_Y     : Integer;
         Section_UI  : Gtk_Sections.Gtk_Section_Map;
         Project     : Aquarius.Projects.Aquarius_Project;
         Ref_Count   : Natural;
         Ref_List    : Aquarius.References.Array_Of_Locations (1 .. Max_Refs);
      end record;

end Aquarius.UI.Gtk_UI;
