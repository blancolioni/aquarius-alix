private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded.Hash;

with Gtk.Fixed;

package Aquarius.UI.Gtk_Sections is

   type Gtk_Section is private;

   type Gtk_Section_Map is private;

   function Create (From_Section  : Aquarius.Sections.Aquarius_Section;
                    Map           : in out Gtk_Section_Map)
                    return Gtk_Section;

   function Exists (Map     : Gtk_Section_Map;
                    Name    : String)
                    return Boolean;

   function Get (Map     : Gtk_Section_Map;
                 Name    : String)
                 return Gtk_Section;

   procedure Render (Item   : Gtk_Section;
                     Target : Gtk.Fixed.Gtk_Fixed;
                     X, Y   : Integer);

   function Displayed
     (Section : Gtk_Section)
      return Boolean;

   procedure Get_Display_Size
     (Section : Gtk_Section;
      Width, Height : out Natural);

private

   type Gtk_Section_Record;

   type Gtk_Section is access all Gtk_Section_Record;

   package Section_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
        Element_Type    => Gtk_Section,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => Ada.Strings.Unbounded."=");

   type Gtk_Section_Map is
      record
         Map : Section_Maps.Map;
      end record;

end Aquarius.UI.Gtk_Sections;
