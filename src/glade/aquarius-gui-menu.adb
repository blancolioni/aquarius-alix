with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);

with Gtk.Menu;
with Gtk.Menu_Item;
with Gtk.Image_Menu_Item;

with Aquarius.GUI.Files;
with Aquarius.GUI.Source;

package body Aquarius.GUI.Menu is

   package Menu_Item_Callback is
      new Gtk.Handlers.Callback
     (Gtk.Image_Menu_Item.Gtk_Image_Menu_Item_Record);

   package Plugin_Menu_Callback is
      new Gtk.Handlers.User_Callback (Gtk.Menu_Item.Gtk_Menu_Item_Record,
                                      Aquarius.UI.Menus.Aquarius_Menu);

   procedure On_File_New
     (M : access Gtk.Image_Menu_Item.Gtk_Image_Menu_Item_Record'Class);

   procedure On_File_Open
     (M : access Gtk.Image_Menu_Item.Gtk_Image_Menu_Item_Record'Class);

   procedure On_Plugin_Menu
     (M       : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      UI_Menu : Aquarius.UI.Menus.Aquarius_Menu);

   Plugin_Menu : Gtk.Menu_Item.Gtk_Menu_Item;

   procedure Create_Menu (Gtk_Menu : Gtk.Menu_Item.Gtk_Menu_Item;
                          UI_Menu  : Aquarius.UI.Menus.Aquarius_Menu);

   -----------------
   -- Create_Menu --
   -----------------

   procedure Create_Menu (Gtk_Menu : Gtk.Menu_Item.Gtk_Menu_Item;
                          UI_Menu  : Aquarius.UI.Menus.Aquarius_Menu)
   is
      use Aquarius.UI.Menus;
   begin

      Aquarius.UI.Menus.Show_Menu (UI_Menu, Current_UI);

      Gtk_Menu.Initialize_With_Mnemonic (Text (UI_Menu));
      if Child_Count (UI_Menu) = 0 then
         Plugin_Menu_Callback.Connect
           (Widget    => Gtk_Menu,
            Name      => "activate",
            Marsh     => Plugin_Menu_Callback.To_Marshaller
              (On_Plugin_Menu'Access),
            User_Data => UI_Menu);
      else
         declare
            Sub_Menu : Gtk.Menu.Gtk_Menu;
         begin
            Gtk.Menu.Gtk_New (Sub_Menu);
            Gtk_Menu.Set_Submenu (Sub_Menu);

            for I in 1 .. Child_Count (UI_Menu) loop
               declare
                  Item : Gtk.Menu_Item.Gtk_Menu_Item;
               begin
                  Gtk.Menu_Item.Gtk_New (Item);
                  Sub_Menu.Append (Item);
                  Create_Menu (Item, Child (UI_Menu, I));
               end;
            end loop;
         end;
      end if;
      Gtk_Menu.Show_All;
   end Create_Menu;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (Builder : Gtk.Builder.Gtk_Builder) is
      use Gtk.Image_Menu_Item;
      File_New : constant Gtk_Image_Menu_Item :=
        Gtk_Image_Menu_Item
          (Builder.Get_Object ("File_New"));
      File_Open : constant Gtk_Image_Menu_Item :=
        Gtk_Image_Menu_Item
          (Builder.Get_Object ("File_Open"));
   begin
      Menu_Item_Callback.Connect (File_New, "activate",
                                  Menu_Item_Callback.To_Marshaller
                                    (On_File_New'Access));
      Menu_Item_Callback.Connect (File_Open, "activate",
                                  Menu_Item_Callback.To_Marshaller
                                    (On_File_Open'Access));
      Plugin_Menu :=
        Gtk.Menu_Item.Gtk_Menu_Item
          (Builder.Get_Object ("PLUGIN"));
   end Initialise;

   -----------------
   -- On_File_New --
   -----------------

   procedure On_File_New
     (M : access Gtk.Image_Menu_Item.Gtk_Image_Menu_Item_Record'Class)
   is
      pragma Unreferenced (M);
   begin
      Aquarius.GUI.Source.New_File ("ada");
   end On_File_New;

   ------------------
   -- On_File_Open --
   ------------------

   procedure On_File_Open
     (M : access Gtk.Image_Menu_Item.Gtk_Image_Menu_Item_Record'Class)
   is
      pragma Unreferenced (M);
   begin
      Aquarius.GUI.Files.Open_File;
   end On_File_Open;

   --------------------
   -- On_Plugin_Menu --
   --------------------

   procedure On_Plugin_Menu
     (M       : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      UI_Menu : Aquarius.UI.Menus.Aquarius_Menu)
   is
      pragma Unreferenced (M);
   begin
      Aquarius.UI.Menus.Activate (UI_Menu);
   end On_Plugin_Menu;

   ----------------------
   -- Show_Plugin_Menu --
   ----------------------

   procedure Show_Plugin_Menu (Menu : Aquarius.UI.Menus.Aquarius_Menu) is
   begin
      Plugin_Menu.Initialize_With_Mnemonic (Aquarius.UI.Menus.Text (Menu));
      Create_Menu (Plugin_Menu, Menu);
   end Show_Plugin_Menu;

end Aquarius.GUI.Menu;
