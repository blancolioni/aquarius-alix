with Glib;

with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);

with Gtk.Layout;
with Gtk.Main;
with Gtk.Widget;
with Gtk.Window;

package body Aquarius.GUI.Manager is

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

   procedure Add_Fragment (Item : Aquarius.GUI.Fragments.Aquarius_Fragment) is
      Widget : constant Gtk.Widget.Gtk_Widget := Item.Create_Widget;
   begin
      Main_Window.Put (Widget, 20, 20);
      Item.Render;
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

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize
     (Layout : access Gtk.Layout.Gtk_Layout_Record'Class)
   is
      pragma Unreferenced (Layout);

      Width, Height : Glib.Gint;
   begin
      Width := Main_Window.Get_Allocation_Width;
      Height := Main_Window.Get_Allocation_Height;
      Local_Current_Position.Bottom :=
        Local_Current_Position.Top + Integer (Height);
      Local_Current_Position.Right :=
        Local_Current_Position.Left + Integer (Width);
   end On_Resize;

end Aquarius.GUI.Manager;
