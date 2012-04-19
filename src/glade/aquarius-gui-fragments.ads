private with Ada.Strings.Unbounded;
private with Gtk.Text_Buffer;

with Aquarius.Fonts;
with Aquarius.Rendering;

with Gtk.Widget;

package Aquarius.GUI.Fragments is

   type Root_Fragment_Type is abstract tagged private;

   function Title (Fragment : Root_Fragment_Type'Class) return String;
   procedure Set_Title (Fragment : in out Root_Fragment_Type'Class;
                        Text     : in     String);

   function Background
     (Fragment : Root_Fragment_Type'Class)
      return Aquarius.Fonts.Aquarius_Colour;

   procedure Set_Background (Fragment : in out Root_Fragment_Type'Class;
                             Background : Aquarius.Fonts.Aquarius_Colour);

   function Create_Widget
     (Fragment : Root_Fragment_Type)
      return Gtk.Widget.Gtk_Widget;

   procedure Render (Fragment : in out Root_Fragment_Type) is abstract;

   type Aquarius_Fragment is access all Root_Fragment_Type'Class;

   function Create_Note_Fragment
     (Width, Height : in     Positive;
      Initial_Text  : in     String)
      return Aquarius_Fragment;

private

   type Root_Fragment_Type is abstract tagged
      record
         Width, Height : Positive;
         Title         : Ada.Strings.Unbounded.Unbounded_String;
         Background    : Aquarius.Fonts.Aquarius_Colour;
         Text_Buffer   : Gtk.Text_Buffer.Gtk_Text_Buffer;
         Renderer      : Aquarius.Rendering.Aquarius_Renderer;
      end record;

end Aquarius.GUI.Fragments;
