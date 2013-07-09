with Glib;

with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);

with Gtk.Layout;
with Gtk.Main;
with Gtk.Widget;
with Gtk.Window;

with Aquarius.Geometry;

package body Aquarius.GUI.Manager is

   type GUI_Layout is
     new Aquarius.Geometry.Layout_Area with null record;

   overriding
   procedure On_Item_Placed
     (Layout : in out GUI_Layout;
      Item   : in out Aquarius.Geometry.Rectangle'Class;
      X, Y   : Integer);

   Fragment_Layout : GUI_Layout;

   type Position is
      record
         Top, Left : Integer;
         Bottom, Right : Integer;
      end record;

   Local_Current_Position : Position;

   Main_Window : Gtk.Layout.Gtk_Layout;
   Pan_Window  : Gtk.Layout.Gtk_Layout;
   pragma Warnings (Off, Pan_Window);

   package Top_Window_Callback is
      new Gtk.Handlers.Callback (Gtk.Window.Gtk_Window_Record);

   package Resize_Cb is
      new Gtk.Handlers.Callback (Gtk.Layout.Gtk_Layout_Record);

   procedure Destroy_Handler (W : access Gtk.Window.Gtk_Window_Record'Class);

   procedure On_Resize
     (Layout : access Gtk.Layout.Gtk_Layout_Record'Class);

   ------------------
   -- Add_Fragment --
   ------------------

   procedure Add_Fragment
     (Item      : Aquarius.Fragments.Aquarius_Fragment;
      Suggest_X : Integer;
      Suggest_Y : Integer)
   is
   begin
      Item.Create_Widget;
      Fragment_Layout.Add_Item (Item, Suggest_X, Suggest_Y);
   end Add_Fragment;

   ---------------------
   -- Destroy_Handler --
   ---------------------

   procedure Destroy_Handler
     (W : access Gtk.Window.Gtk_Window_Record'Class)
   is
      pragma Unreferenced (W);
   begin
      Gtk.Main.Main_Quit;
   end Destroy_Handler;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (Builder : Gtk.Builder.Gtk_Builder) is
      Top_Window : constant Gtk.Widget.Gtk_Widget :=
                     Gtk.Widget.Gtk_Widget
                       (Builder.Get_Object ("Aquarius_Main"));
   begin
      Top_Window_Callback.Connect
        (Gtk.Window.Gtk_Window (Top_Window),
         "destroy",
         Top_Window_Callback.To_Marshaller (Destroy_Handler'Access));
      Main_Window :=
        Gtk.Layout.Gtk_Layout (Builder.Get_Object ("Fragment_Window"));
      Pan_Window :=
        Gtk.Layout.Gtk_Layout (Builder.Get_Object ("Pan_Window"));
      Main_Window.Set_Size (1_000, 300);
      Resize_Cb.Connect (Main_Window, "size_allocate",
                         On_Resize'Access);
   end Initialise;

   overriding
   procedure On_Item_Placed
     (Layout : in out GUI_Layout;
      Item   : in out Aquarius.Geometry.Rectangle'Class;
      X, Y   : Integer)
   is
      pragma Unreferenced (Layout);
      Fragment : Aquarius.Fragments.Root_Fragment_Type'Class renames
                   Aquarius.Fragments.Root_Fragment_Type'Class (Item);
   begin
      Main_Window.Put (Fragment.Widget, Glib.Gint (X), Glib.Gint (Y));
      Fragment.Render;
   end On_Item_Placed;

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize
     (Layout : access Gtk.Layout.Gtk_Layout_Record'Class)
   is
      pragma Unreferenced (Layout);

      Width, Height : Glib.Guint;
   begin
      Main_Window.Get_Size (Width, Height);
      Local_Current_Position.Bottom :=
        Local_Current_Position.Top + Integer (Height);
      Local_Current_Position.Right :=
        Local_Current_Position.Left + Integer (Width);
   end On_Resize;

end Aquarius.GUI.Manager;
