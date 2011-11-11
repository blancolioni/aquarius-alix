with Glib.Error;

package body Gtk.Builder is

   -------------------
   -- Add_From_File --
   -------------------

   procedure Add_From_File
     (Builder   : access Gtk_Builder_Record;
      File_Name : in     UTF8_String)
   is
      function Internal
        (Builder   : System.Address;
         File_Name : String;
         Error     : Glib.Error.GError_Access)
        return Glib.Gint;
      pragma Import (C, Internal, "gtk_builder_add_from_file");

      Error : aliased Glib.Error.GError;
      Result : constant Glib.Gint :=
        Internal (Get_Object (Builder), File_Name & ASCII.NUL,
                  Error'Unchecked_Access);
   begin
      if Result = 0 then
         raise Program_Error with
           "Error loading UI: " & Glib.Error.Get_Message (Error);
      end if;
   end Add_From_File;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object
     (Builder   : access Gtk_Builder_Record;
      Name      : in     UTF8_String)
      return Gtk.Object.Gtk_Object
   is
      function Internal
        (Builder : System.Address;
         Name    : String)
        return System.Address;
      pragma Import (C, Internal, "gtk_builder_get_object");
      Stub : Gtk.Object.Gtk_Object_Record;
   begin
      return Gtk.Object.Gtk_Object
        (Get_User_Data
          (Internal (Get_Object (Builder), Name & ASCII.NUL), Stub));
   end Get_Object;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Builder      : out Gtk_Builder)
   is
   begin
      Builder := new Gtk_Builder_Record;
      Gtk.Builder.Initialize (Builder);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Builder  : access Gtk_Builder_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_builder_new");
   begin
      Set_Object (Builder, Internal);
   end Initialize;

end Gtk.Builder;
