with Ada.Text_IO;
with Ada.Integer_Text_IO;

with Glib.Error;

--  with Gdk.Color;

with Gtk.Builder;
with Gtk.Cell_Renderer_Text;
with Gtk.Editable;
with Gtk.Handlers;
with Gtk.Main;
with Gtk.Tree_Model;
with Gtk.Tree_Selection;
with Gtk.Tree_Store;
with Gtk.Tree_View_Column;
with Gtk.Widget;
with Gtk.Window;

--  with Aquarius.Colours.Gtk;
with Aquarius.Config_Paths;
with Aquarius.Programs.Arrangements;
with Aquarius.Rendering.Sections;
with Aquarius.Sections.Code;
with Aquarius.Trees.Cursors;
with Aquarius.UI.Gtk_Text;

package body Aquarius.UI.Gtk_UI is

   package Main_Window_Callback is
     new Gtk.Handlers.Callback (Gtk.Window.Gtk_Window_Record);

   package Drawing_Area_UI_Callback is
     new Gtk.Handlers.User_Return_Callback
       (Widget_Type => Gtk.Drawing_Area.Gtk_Drawing_Area_Record,
        Return_Type => Boolean,
        User_Type   => Aquarius_UI);

   package Search_Text_Changed_Callback is
     new Gtk.Handlers.User_Callback
       (Widget_Type => Gtk.GEntry.Gtk_Entry_Record,
        User_Type   => Aquarius_UI);

   package Select_Item_Callback is
     new Gtk.Handlers.User_Callback
       (Widget_Type => Gtk.Tree_View.Gtk_Tree_View_Record,
        User_Type   => Aquarius_UI);

   procedure Destroy_Handler (W : access Gtk.Window.Gtk_Window_Record'Class);

   function Configure_Overview_Handler
     (W       : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      With_UI : Aquarius_UI)
      return Boolean;

   function Expose_Overview_Handler
     (W       : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      With_UI : Aquarius_UI)
      return Boolean;

   procedure On_Search_Text_Changed
     (W       : access Gtk.GEntry.Gtk_Entry_Record'Class;
      With_UI : Aquarius_UI);

   procedure On_Reference_Activated
     (Widget  : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      With_UI : Aquarius_UI);

   procedure Render_Overview (With_UI : in out Gtk_UI'Class);
   procedure Render_Sections (With_UI : in out Gtk_UI'Class);

   procedure Update_Identifiers
     (With_UI : in out Gtk_UI'Class);

   --------------------------------
   -- Configure_Overview_Handler --
   --------------------------------

   function Configure_Overview_Handler
     (W       : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      With_UI : Aquarius_UI)
      return Boolean
   is
      pragma Unreferenced (W);
   begin
      Render_Overview (Gtk_UI (With_UI.all));
      return True;
   end Configure_Overview_Handler;

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

   -----------------------------
   -- Expose_Overview_Handler --
   -----------------------------

   function Expose_Overview_Handler
     (W       : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      With_UI : Aquarius_UI)
      return Boolean
   is
      pragma Unreferenced (W);
   begin
      Render_Overview (Gtk_UI (With_UI.all));
      return True;
   end Expose_Overview_Handler;

   ----------
   -- Init --
   ----------

   overriding
   procedure Init (With_UI : not null access Gtk_UI) is
      Builder : Gtk.Builder.Gtk_Builder;
      UI_Path : constant String :=
                  Aquarius.Config_Paths.Config_Path & "/gui/aquarius-gtk.ui";
   begin

      Gtk.Main.Init;

      Gtk.Builder.Gtk_New (Builder);

      Ada.Text_IO.Put_Line ("Loading: " & UI_Path);

      declare
         use Glib;
         use type Glib.Error.GError;
         Error : aliased Glib.Error.GError;
         Result : Guint;
      begin
         Result :=
           Gtk.Builder.Add_From_File (Builder, UI_Path, Error'Access);
         if Result = 0 then
            raise Program_Error with
              "Error opening GUI definition: " & UI_Path;
         end if;
      end;

      Ada.Text_IO.Put_Line ("done");

      declare
         Main_Window : constant Gtk.Widget.Gtk_Widget :=
                         Gtk.Widget.Gtk_Widget
                           (Builder.Get_Object ("Aquarius_Main_Window"));
      begin
         Main_Window_Callback.Connect
           (Gtk.Window.Gtk_Window (Main_Window),
            "destroy",
            Main_Window_Callback.To_Marshaller (Destroy_Handler'Access));
         Main_Window.Show_All;
      end;

      Aquarius.Sections.Layout.Initialise
        (With_UI.Layout, 10_000, 2_000);

      With_UI.Overview :=
        Gtk.Drawing_Area.Gtk_Drawing_Area
          (Builder.Get_Object ("Overview"));
      With_UI.Sections :=
        Gtk.Fixed.Gtk_Fixed
          (Builder.Get_Object ("Section_Area"));

      Drawing_Area_UI_Callback.Connect
        (With_UI.Overview, Gtk.Widget.Signal_Visibility_Notify_Event,
         Drawing_Area_UI_Callback.To_Marshaller
           (Expose_Overview_Handler'Access),
         Aquarius_UI (With_UI));
      Drawing_Area_UI_Callback.Connect
        (With_UI.Overview, "configure-event",
         Drawing_Area_UI_Callback.To_Marshaller
           (Configure_Overview_Handler'Access),
         Aquarius_UI (With_UI));

      Aquarius.UI.Gtk_Text.Initialise;

      With_UI.Start_X := 0;
      With_UI.Start_Y := 0;

      With_UI.Identifiers :=
        Gtk.Tree_View.Gtk_Tree_View
          (Builder.Get_Object ("Identifiers"));

      With_UI.Search :=
        Gtk.GEntry.Gtk_Entry
          (Builder.Get_Object ("Search"));

      Select_Item_Callback.Connect
        (With_UI.Identifiers, Gtk.Tree_View.Signal_Row_Activated,
         Select_Item_Callback.To_Marshaller
           (On_Reference_Activated'Access),
         With_UI);

      Search_Text_Changed_Callback.Connect
        (With_UI.Search, Gtk.Editable.Signal_Changed,
         Search_Text_Changed_Callback.To_Marshaller
           (On_Search_Text_Changed'Access),
         Aquarius_UI (With_UI));

   end Init;

   ----------------------------
   -- On_Reference_Activated --
   ----------------------------

   procedure On_Reference_Activated
     (Widget  : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      With_UI : Aquarius_UI)
   is
      Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Selection : constant Gtk.Tree_Selection.Gtk_Tree_Selection :=
        Widget.Get_Selection;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Gtk.Tree_Selection.Get_Selected (Selection, Model, Iter);
      declare
         Index : constant Natural :=
                   Natural (Gtk.Tree_Model.Get_Int (Model, Iter, 2));
      begin
         if Index > 0 then
            declare
               Program : constant Aquarius.Programs.Program_Tree :=
                           Aquarius.References.Reference_Program
                             (Gtk_UI (With_UI.all).Ref_List (Index));
               Section : constant Aquarius.Sections.Aquarius_Section :=
                           Aquarius.Sections.Code.New_Code_Section
                             (Program.Show_Location);
               Renderer : constant Aquarius.Rendering.Aquarius_Renderer :=
                            Aquarius.Rendering.Sections.New_Section_Renderer
                              (Section);
            begin
               Aquarius.Programs.Arrangements.Arrange
                 (Item        => Program,
                  Line_Length => 40);

               Aquarius.Programs.Arrangements.Render
                 (Program  => Program,
                  Renderer => Renderer,
                  Point    => Aquarius.Trees.Cursors.Left_Of_Tree (Program),
                  Partial  => "");
               With_UI.Show_Section (Section, 200, 0);
               Render_Sections (Gtk_UI (With_UI.all));
            end;
         end if;
      end;
   end On_Reference_Activated;

   ----------------------------
   -- On_Search_Text_Changed --
   ----------------------------

   procedure On_Search_Text_Changed
     (W       : access Gtk.GEntry.Gtk_Entry_Record'Class;
      With_UI : Aquarius_UI)
   is
      pragma Unreferenced (W);
   begin
      Update_Identifiers (Gtk_UI (With_UI.all));
   end On_Search_Text_Changed;

   ---------------------
   -- Render_Overview --
   ---------------------

   procedure Render_Overview (With_UI : in out Gtk_UI'Class) is

--        Window : constant Gdk.Gdk_Window :=
--                   With_UI.Overview.Get_Window;
--        GC     : Gdk.Gdk_GC;

      procedure Render (Section       : Aquarius.Sections.Aquarius_Section;
                        X, Y          : Integer);

      ------------
      -- Render --
      ------------

      procedure Render (Section       : Aquarius.Sections.Aquarius_Section;
                        X, Y          : Integer)
      is
--           Bg : Gdk.Color.Gdk_Color :=
--                  Aquarius.Colours.Gtk.To_Gdk_Color (Section.Background);
--           Success : Boolean;
      begin
         Ada.Text_IO.Put ("overview: " & Section.Id & " at (");
         Ada.Integer_Text_IO.Put (X, 1);
         Ada.Text_IO.Put (",");
         Ada.Integer_Text_IO.Put (Y, 1);
         Ada.Text_IO.Put (") ");
         Ada.Integer_Text_IO.Put (Section.Render_Width, 1);
         Ada.Text_IO.Put ("x");
         Ada.Integer_Text_IO.Put (Section.Render_Height, 1);
         Ada.Text_IO.New_Line;

--           Gdk.GC.Set_Foreground
--             (GC, Bg);
--           Gdk.Drawable.Draw_Rectangle
--             (Window, GC, True,
--              Glib.Gint (X / 10),
--              Glib.Gint (Y / 10),
--              Glib.Gint (Section.Render_Width / 10),
--              Glib.Gint (Section.Render_Height / 10));
      end Render;

   begin
      --  Gdk.GC.Gdk_New (GC, Window);
      Aquarius.Sections.Layout.Render_Overview
        (Layout   => With_UI.Layout,
         Renderer => Render'Access);
   end Render_Overview;

   ---------------------
   -- Render_Sections --
   ---------------------

   procedure Render_Sections (With_UI : in out Gtk_UI'Class) is

      procedure Render (Section : Aquarius.Sections.Aquarius_Section;
                        X, Y    : Integer);

      ------------
      -- Render --
      ------------

      procedure Render
        (Section : Aquarius.Sections.Aquarius_Section;
         X, Y    : Integer)
      is
         use Aquarius.UI.Gtk_Sections;
         Item : Gtk_Section;
      begin
         if Exists (With_UI.Section_UI, Section.Id) then
            Item := Get (With_UI.Section_UI, Section.Id);
         else
            Item := Create (Section, With_UI.Section_UI);
         end if;

         Render (Item, With_UI.Sections,
                 X - With_UI.Start_X,
                 Y - With_UI.Start_Y);

      end Render;

   begin
      Aquarius.Sections.Layout.Render
        (Layout   => With_UI.Layout,
         X_Min    => With_UI.Start_X,
         X_Max    => With_UI.Start_X + 1200,
         Y_Min    => With_UI.Start_Y,
         Y_Max    => With_UI.Start_Y + 800,
         Renderer => Render'Access);

      With_UI.Overview.Queue_Draw;

   end Render_Sections;

   ------------------
   -- Show_Project --
   ------------------

   overriding
   procedure Show_Project
     (User_Interface : in out Gtk_UI;
      Project        : not null access
        Aquarius.Projects.Aquarius_Project_Type'Class)
   is
      Model       : Gtk.Tree_Store.Gtk_Tree_Store;
      Text_Render : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Name_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Loc_Column  : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Num         : Glib.Gint;
      pragma Warnings (Off, Num);
   begin
      User_Interface.Project :=
        Aquarius.Projects.Aquarius_Project (Project);
      Gtk.Tree_Store.Gtk_New (Model,
        (0     => Glib.GType_String,
         1     => Glib.GType_String,
         2     => Glib.GType_Int));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Render);
      Gtk.Tree_View_Column.Gtk_New (Name_Column);
      Num := User_Interface.Identifiers.Append_Column (Name_Column);
      Gtk.Tree_View_Column.Gtk_New (Loc_Column);
      Num := User_Interface.Identifiers.Append_Column (Loc_Column);
      Name_Column.Pack_Start (Text_Render, True);
      Name_Column.Set_Sizing (Gtk.Tree_View_Column.Tree_View_Column_Autosize);
      Name_Column.Add_Attribute (Text_Render, "text", 0);
      Loc_Column.Pack_Start (Text_Render, True);
      Loc_Column.Set_Sizing (Gtk.Tree_View_Column.Tree_View_Column_Autosize);
      Loc_Column.Add_Attribute (Text_Render, "text", 1);
      User_Interface.Identifiers.Set_Model (Model.To_Interface);
--        (Gtk.Tree_Model.Gtk_Tree_Model (Model));
      Update_Identifiers (User_Interface);
   end Show_Project;

   ------------------
   -- Show_Section --
   ------------------

   overriding
   procedure Show_Section
     (On      : in out Gtk_UI;
      Section : Aquarius.Sections.Aquarius_Section;
      Hint_X  : Integer;
      Hint_Y  : Integer)
   is
   begin
      Aquarius.Sections.Layout.Show_Section
        (On.Layout, Section, Hint_X, Hint_Y);
   end Show_Section;

   -----------
   -- Start --
   -----------

   overriding
   procedure Start (With_UI : in out Gtk_UI) is
   begin

      Render_Sections (With_UI);

      Gtk.Main.Main;

   end Start;

   ------------------------
   -- Update_Identifiers --
   ------------------------

   procedure Update_Identifiers
     (With_UI : in out Gtk_UI'Class)
   is
      use type Gtk.Tree_Model.Gtk_Tree_Model;
      Refs : constant Aquarius.References.Array_Of_Locations :=
               Aquarius.References.Filter
                 (List => With_UI.Project.References,
                  Text => With_UI.Search.Get_Text,
                  Max  => Max_Refs);
      Store : constant Gtk.Tree_Store.Gtk_Tree_Store :=
                Gtk.Tree_Store.Gtk_Tree_Store
                  (-With_UI.Identifiers.Get_Model);
   begin
      Store.Clear;
      for I in Refs'Range loop
         declare
            Parent : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                       Gtk.Tree_Model.Null_Iter;
            Result : Gtk.Tree_Model.Gtk_Tree_Iter;
            Cursor : constant Aquarius.References.Reference_Cursor :=
                       Refs (I);
         begin
            Store.Append (Result, Parent);
            Store.Set (Result, 0,
                       Aquarius.References.Reference_Name (Cursor));
            Store.Set
              (Result, 1,
               Aquarius.References.Reference_Program (Cursor).Name);
            Store.Set (Result, 2, Glib.Gint (I));
         end;
      end loop;

      With_UI.Ref_List (Refs'Range) := Refs;
      With_UI.Ref_Count := Refs'Length;

   end Update_Identifiers;

end Aquarius.UI.Gtk_UI;
