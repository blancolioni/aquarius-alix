package body Aquarius.Fragments is

   ----------------
   -- Background --
   ----------------

   function Background
     (Fragment : Root_Fragment_Type'Class)
      return Aquarius.Fonts.Aquarius_Colour
   is
   begin
      return Fragment.Background;
   end Background;

   ------------
   -- Height --
   ------------

   overriding function Height
     (Fragment : Root_Fragment_Type)
      return Positive
   is
   begin
      return Fragment.Height;
   end Height;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise
     (Fragment      : in out Root_Fragment_Type'Class;
      Width, Height : Positive)
   is
   begin
      Fragment.Width := Width;
      Fragment.Height := Height;
   end Initialise;

   --------------
   -- Renderer --
   --------------

   function Renderer
     (Fragment : Root_Fragment_Type'Class)
      return Aquarius.Rendering.Aquarius_Renderer
   is
   begin
      return Fragment.Renderer;
   end Renderer;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Fragment : in out Root_Fragment_Type'Class;
      Background : Aquarius.Fonts.Aquarius_Colour)
   is
   begin
      Fragment.Background := Background;
   end Set_Background;

   ------------------
   -- Set_Position --
   ------------------

--     overriding procedure Set_Position
--       (Fragment : in out Root_Fragment_Type;
--        X, Y     : in     Integer)
--     is
--     begin
--
--     end Set_Position;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Fragment : in out Root_Fragment_Type'Class;
      Text     : in     String)
   is
   begin
      Fragment.Title := Ada.Strings.Unbounded.To_Unbounded_String (Text);
   end Set_Title;

   -----------
   -- Title --
   -----------

   function Title (Fragment : Root_Fragment_Type'Class) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Fragment.Title);
   end Title;

   ------------
   -- Widget --
   ------------

   function Widget
     (Fragment : Root_Fragment_Type'Class)
      return Gtk.Widget.Gtk_Widget
   is
   begin
      return Fragment.Widget;
   end Widget;

   -----------
   -- Width --
   -----------

   overriding function Width
     (Fragment : Root_Fragment_Type)
      return Positive
   is
   begin
      return Fragment.Width;
   end Width;

end Aquarius.Fragments;
