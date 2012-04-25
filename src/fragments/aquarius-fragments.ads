private with Ada.Strings.Unbounded;

with Gtk.Widget;

with Aquarius.Fonts;
with Aquarius.Geometry;
with Aquarius.Keys;
with Aquarius.Layout;
with Aquarius.Rendering;

package Aquarius.Fragments is

   type Root_Fragment_Type is
     abstract new Aquarius.Geometry.Rectangle
   with private;

   function Title (Fragment : Root_Fragment_Type'Class) return String;
   procedure Set_Title (Fragment : in out Root_Fragment_Type'Class;
                        Text     : in     String);

   function Background
     (Fragment : Root_Fragment_Type'Class)
      return Aquarius.Fonts.Aquarius_Colour;

   procedure Set_Background (Fragment : in out Root_Fragment_Type'Class;
                             Background : Aquarius.Fonts.Aquarius_Colour);

   procedure Render (Fragment : in out Root_Fragment_Type)
   is abstract;

   overriding
   function Width (Fragment : Root_Fragment_Type) return Positive;

   overriding
   function Height (Fragment : Root_Fragment_Type) return Positive;

   function On_Key_Press (Fragment : in out Root_Fragment_Type;
                          Position : in     Aquarius.Layout.Position;
                          Key      : in     Aquarius.Keys.Aquarius_Key)
                          return Boolean
                          is abstract;
   --  Handle the key.  Return True if it was fully handled, false if
   --  not (in which case it is passed to the default handler)

--     overriding
--     procedure Set_Position (Fragment : in out Root_Fragment_Type;
--                             X, Y     : in     Integer);

   procedure Create_Widget
     (Fragment : not null access Root_Fragment_Type)
   is abstract;

   function Widget
     (Fragment : Root_Fragment_Type'Class)
      return Gtk.Widget.Gtk_Widget;

   procedure Initialise
     (Fragment      : in out Root_Fragment_Type'Class;
      Width, Height : Positive);

   function Renderer (Fragment : Root_Fragment_Type'Class)
                      return Aquarius.Rendering.Aquarius_Renderer;

   type Aquarius_Fragment is access all Root_Fragment_Type'Class;

private

   type Root_Fragment_Type is
     abstract new Aquarius.Geometry.Rectangle with
      record
         Width, Height : Positive;
         Title         : Ada.Strings.Unbounded.Unbounded_String;
         Background    : Aquarius.Fonts.Aquarius_Colour;
         Renderer      : Aquarius.Rendering.Aquarius_Renderer;
         Widget        : Gtk.Widget.Gtk_Widget;
      end record;

end Aquarius.Fragments;
