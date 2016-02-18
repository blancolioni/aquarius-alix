with Glib.Object;
with Gdk.Event;

with Gtk.Widget;

with Cairo;

with Komnenos.UI.Gtk_UI.Connectors;

package body Komnenos.UI.Gtk_UI.Layout_Table is

   type Layout_Object_Record is
     new Glib.Object.GObject_Record with
      record
         Layout : Gtk_Layout_Table;
      end record;

   type Layout_Object_Access is access all Layout_Object_Record'Class;

   function On_Configure_Layout
     (Object : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Configure)
      return Boolean;

   function On_Draw_Layout
     (Object : access Glib.Object.GObject_Record'Class;
      Cr     : Cairo.Cairo_Context)
      return Boolean;

   procedure On_Navigation
     (Layout            : Komnenos.Layouts.Layout_Type;
      New_Left, New_Top : Natural);

   procedure Configure
     (Layout : Gtk_Layout_Table);

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Layout : Gtk_Layout_Table)
   is
      Size : Gtk.Widget.Gtk_Allocation;
   begin
      Layout.Layout_Widget.Get_Allocation
        (Size);
      Layout.Set_Visible_Area
        (Layout.Visible_Left,
         Layout.Visible_Top,
         Positive (Size.Width),
         Positive (Size.Height));
      Layout.Navigation.On_Layout_Configured;
      Layout.Navigation.Refresh;
   end Configure;

   ----------------
   -- Connection --
   ----------------

   overriding procedure Connection
     (Layout    : in out Root_Gtk_Layout_Table;
      Connector : Komnenos.Connectors.Connector_Type)
   is
      UI_Connector : constant Komnenos.UI.Gtk_UI.Connectors.Gtk_Connector :=
                       Komnenos.UI.Gtk_UI.Connectors.New_Connector
                         (Connector);
      Loc : constant Layout_Point := UI_Connector.Layout_Location;
   begin
      Layout.Layout_Widget.Put
        (UI_Connector,
         Glib.Gint (Loc.X),
         Glib.Gint (Loc.Y));
   end Connection;

   -------------------------
   -- Create_Layout_Table --
   -------------------------

   function Create_Layout_Table
     (Navigation    : Komnenos.UI.Gtk_UI.Navigation.Gtk_Navigation_Panel;
      Main_Scroll   : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Main_Layout   : Gtk.Layout.Gtk_Layout)
      return Gtk_Layout_Table
   is
      pragma Unreferenced (Main_Scroll);
      Result : constant Gtk_Layout_Table :=
                 new Root_Gtk_Layout_Table'
                   (Komnenos.Layouts.Root_Layout_Type with
                    Self          => null,
                    Navigation    => Navigation,
                    Layout_Widget => Main_Layout,
                    Frame_Map     => Frames.Maps.Empty_Map);
      Object : constant Layout_Object_Access :=
                 new Layout_Object_Record'
                   (Glib.Object.GObject_Record with Layout => Result);
   begin
      Object.Initialize;
      Result.Self := Komnenos.Layouts.Layout_Type (Result);

      if False then
         Main_Layout.On_Configure_Event
           (On_Configure_Layout'Access,
            Object);
      end if;

      Main_Layout.On_Draw
        (On_Draw_Layout'Access, Object);

      Main_Layout.Set_Size (12_000, 1200);
      Result.Navigation.Set_Layout (Result);
      Result.Navigation.On_Navigate (On_Navigation'Access);
      return Result;
   end Create_Layout_Table;

   ---------------
   -- Get_Frame --
   ---------------

   function Get_Frame
     (Layout   : Root_Gtk_Layout_Table'Class;
      Fragment : Komnenos.Fragments.Fragment_Type)
      return Komnenos.UI.Gtk_UI.Frames.Gtk_Frame
   is
   begin
      return Layout.Frame_Map.Element (Fragment.Key);
   end Get_Frame;

   ----------------
   -- Item_Moved --
   ----------------

   overriding procedure Item_Moved
     (Layout : in out Root_Gtk_Layout_Table;
      Item   : Komnenos.Fragments.Fragment_Type)
   is
      Frame : constant Frames.Gtk_Frame :=
                Layout.Get_Frame (Item);
   begin
      Layout.Layout_Widget.Move
        (Frame, Glib.Gint (Item.X), Glib.Gint (Item.Y));
   end Item_Moved;

   -----------------
   -- Item_Placed --
   -----------------

   overriding procedure Item_Placed
     (Layout : in out Root_Gtk_Layout_Table;
      Item   : Komnenos.Fragments.Fragment_Type)
   is
      Frame : constant Komnenos.UI.Gtk_UI.Frames.Gtk_Frame :=
                Komnenos.UI.Gtk_UI.Frames.New_Frame
                  (Layout   => Layout.Self,
                   Fragment => Item);
   begin
      Layout.Layout_Widget.Put
        (Frame, Glib.Gint (Item.X), Glib.Gint (Item.Y));
      Layout.Frame_Map.Insert (Item.Key, Frame);
   end Item_Placed;

   ------------------
   -- Item_Removed --
   ------------------

   overriding procedure Item_Removed
     (Layout : in out Root_Gtk_Layout_Table;
      Item   : Komnenos.Fragments.Fragment_Type)
   is
      Frame : constant Komnenos.UI.Gtk_UI.Frames.Gtk_Frame :=
                Layout.Frame_Map.Element (Item.Key);
   begin
      Layout.Layout_Widget.Remove (Frame);
      Layout.Frame_Map.Delete (Item.Key);
   end Item_Removed;

   -------------------------
   -- On_Configure_Layout --
   -------------------------

   function On_Configure_Layout
     (Object : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Configure)
      return Boolean
   is
      pragma Unreferenced (Event);
   begin
      Configure (Layout_Object_Access (Object).Layout);
      return False;
   end On_Configure_Layout;

   --------------------
   -- On_Draw_Layout --
   --------------------

   function On_Draw_Layout
     (Object : access Glib.Object.GObject_Record'Class;
      Cr     : Cairo.Cairo_Context)
      return Boolean
   is
      pragma Unreferenced (Cr);
   begin
      Configure (Layout_Object_Access (Object).Layout);
      return False;
   end On_Draw_Layout;

   -------------------
   -- On_Navigation --
   -------------------

   procedure On_Navigation
     (Layout            : Komnenos.Layouts.Layout_Type;
      New_Left, New_Top : Natural)
   is
      Gtk_Layout : constant Gtk_Layout_Table := Gtk_Layout_Table (Layout);
   begin
      Gtk_Layout.Set_Top_Left (New_Left, New_Top);
   end On_Navigation;

   -------------------
   -- Set_Full_Size --
   -------------------

   overriding procedure Set_Full_Size
     (Layout      : in out Root_Gtk_Layout_Table;
      Full_Width  : Natural;
      Full_Height : Natural)
   is
   begin
      Komnenos.Layouts.Root_Layout_Type (Layout).Set_Full_Size
        (Full_Width, Full_Height);
      Layout.Layout_Widget.Set_Size
        (Glib.Guint (Full_Width), Glib.Guint (Full_Height));
   end Set_Full_Size;

   ------------------
   -- Set_Top_Left --
   ------------------

   procedure Set_Top_Left
     (Table : in out Root_Gtk_Layout_Table;
      X, Y  : Natural)
   is
   begin
      Table.Layout_Widget.Get_Hadjustment.Set_Value (Glib.Gdouble (X));
      Table.Layout_Widget.Get_Vadjustment.Set_Value (Glib.Gdouble (Y));
   end Set_Top_Left;

   ----------------------
   -- Update_Connector --
   ----------------------

   overriding procedure Update_Connector
     (Layout    : in out Root_Gtk_Layout_Table;
      Connector : Komnenos.Connectors.Connector_Type)
   is
      Conn : constant Connectors.Gtk_Connector :=
               Connectors.Gtk_Connector (Connector.Display);
   begin
      Layout.Layout_Widget.Move
        (Conn,
         Glib.Gint (Conn.Layout_Location.X),
         Glib.Gint (Conn.Layout_Location.Y));
   end Update_Connector;

end Komnenos.UI.Gtk_UI.Layout_Table;
