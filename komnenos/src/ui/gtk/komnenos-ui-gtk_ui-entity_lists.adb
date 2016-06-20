with System.Address_To_Access_Conversions;

with Ada.Text_IO;

with Glib;
with Glib.Values;

with Gdk.Event;

with Gtk.Cell_Renderer_Text;
with Gtk.Editable;
with Gtk.List_Store;
with Gtk.Handlers;
with Gtk.Main;
with Gtk.Menu;
with Gtk.Menu_Item;
with Gtk.Tree_Model;
with Gtk.Tree_Selection;
with Gtk.Tree_View_Column;
with Gtk.Widget;

with Komnenos.Entities.Source.Aquarius_Source;

package body Komnenos.UI.Gtk_UI.Entity_Lists is

   Popup_Menu : Gtk.Menu.Gtk_Menu;

   package Entity_Tree_To_Access is
     new System.Address_To_Access_Conversions
       (Komnenos.Entities.Root_Entity_Reference'Class);

   package Filter_Callback is
     new Gtk.Handlers.User_Callback
       (Widget_Type => Gtk.Widget.Gtk_Widget_Record,
        User_Type => Gtk.Tree_View.Gtk_Tree_View);

   package Tree_Callback is
     new Gtk.Handlers.Callback (Gtk.Tree_View.Gtk_Tree_View_Record);

   package Context_Menu_Reference_Callback is
     new Gtk.Handlers.User_Callback
       (Widget_Type => Gtk.Menu_Item.Gtk_Menu_Item_Record,
        User_Type   => Komnenos.Entities.Reference_Record);

   package Context_Menu_Entity_Callback is
     new Gtk.Handlers.User_Callback
       (Widget_Type => Gtk.Menu_Item.Gtk_Menu_Item_Record,
        User_Type   => Komnenos.Entities.Entity_Reference);

   procedure On_Filter_Changed
     (W : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tree_View : Gtk.Tree_View.Gtk_Tree_View);

   procedure On_Row_Activated
     (Tree_View : access Gtk.Tree_View.Gtk_Tree_View_Record'Class);

   function On_Tree_View_Button_Press
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean;

   procedure On_Context_Menu_Item_Selected
     (Widget : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      Reference : Komnenos.Entities.Reference_Record);

   procedure On_Show_Syntax
     (Widget : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      Entity : Komnenos.Entities.Entity_Reference);

   function Get_Selected_Entity
     (Tree_View : Gtk.Tree_View.Gtk_Tree_View)
      return Komnenos.Entities.Entity_Reference;

   procedure Show_Context_Menu
     (Tree_View  : Gtk.Tree_View.Gtk_Tree_View;
      Event      : Gdk.Event.Gdk_Event_Button;
      Target     : Komnenos.Entities.Entity_Reference;
      References : Komnenos.Entities.Reference_Record_Array);

   procedure Show_Entities
     (Tree_View : Gtk.Tree_View.Gtk_Tree_View;
      Filter    : String);

   ------------
   -- Create --
   ------------

   procedure Create
     (Tree_View : Gtk.Tree_View.Gtk_Tree_View;
      Filter    : Gtk.GEntry.Gtk_Entry)
   is
      Model : Gtk.List_Store.Gtk_List_Store;
   begin
      Filter_Callback.Connect
        (Filter, Gtk.Editable.Signal_Changed,
         Filter_Callback.To_Marshaller (On_Filter_Changed'Access),
         Tree_View);

      Gtk.List_Store.Gtk_New
        (List_Store => Model,
         Types      =>
           (0 => Glib.GType_String,
            1 => Glib.GType_Pointer,
            2 => Glib.GType_String));

      Tree_View.Set_Model (Model.To_Interface);
      Tree_View.Set_Tooltip_Column (2);

      Tree_Callback.Connect
        (Tree_View, Gtk.Tree_View.Signal_Row_Activated,
         Tree_Callback.To_Marshaller
           (On_Row_Activated'Access));

      Tree_View.On_Button_Press_Event
        (On_Tree_View_Button_Press'Access);

      declare
         Text_Render : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
         Text_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
         Num         : Glib.Gint;
         pragma Unreferenced (Num);
      begin
         Gtk.Cell_Renderer_Text.Gtk_New (Text_Render);
         Gtk.Tree_View_Column.Gtk_New (Text_Column);
         Num := Tree_View.Append_Column (Text_Column);
         Text_Column.Pack_Start (Text_Render, True);
         Text_Column.Set_Sizing
           (Gtk.Tree_View_Column.Tree_View_Column_Autosize);
         Text_Column.Add_Attribute (Text_Render, "text", 0);
      end;

      Show_Entities (Tree_View, "");

   end Create;

   -------------------------
   -- Get_Selected_Entity --
   -------------------------

   function Get_Selected_Entity
     (Tree_View : Gtk.Tree_View.Gtk_Tree_View)
      return Komnenos.Entities.Entity_Reference
   is
      Model     : Gtk.Tree_Model.Gtk_Tree_Model;
      Selection : constant Gtk.Tree_Selection.Gtk_Tree_Selection :=
                    Tree_View.Get_Selection;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Gtk.Tree_Selection.Get_Selected (Selection, Model, Iter);

      declare
         Entity_Addr : constant System.Address :=
                         Gtk.Tree_Model.Get_Address
                           (Model, Iter, 1);
         Entity      : constant Komnenos.Entities.Entity_Reference :=
                         Komnenos.Entities.Entity_Reference
                           (Entity_Tree_To_Access.To_Pointer
                              (Entity_Addr));
      begin
         return Entity;
      end;
   end Get_Selected_Entity;

   -----------------------------------
   -- On_Context_Menu_Item_Selected --
   -----------------------------------

   procedure On_Context_Menu_Item_Selected
     (Widget : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      Reference : Komnenos.Entities.Reference_Record)
   is
      use Komnenos.Entities, Komnenos.Entities.Source.Aquarius_Source;
      pragma Unreferenced (Widget);
      Entity : constant Entity_Reference :=
                 Komnenos.Entities.Get_Referrer (Reference);
   begin
      if Entity /= null then
         Entity.Select_Entity (Current_UI, null, null, 0);
      end if;
   end On_Context_Menu_Item_Selected;

   -----------------------
   -- On_Filter_Changed --
   -----------------------

   procedure On_Filter_Changed
     (W : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tree_View : Gtk.Tree_View.Gtk_Tree_View)
   is
   begin
      Show_Entities (Tree_View, Gtk.GEntry.Gtk_Entry (W).Get_Chars (0));
   end On_Filter_Changed;

   ----------------------
   -- On_Row_Activated --
   ----------------------

   procedure On_Row_Activated
     (Tree_View : access Gtk.Tree_View.Gtk_Tree_View_Record'Class)
   is
      Entity : constant Komnenos.Entities.Entity_Reference :=
                 Get_Selected_Entity
                   (Gtk.Tree_View.Gtk_Tree_View (Tree_View));
   begin
      Entity.Select_Entity (Current_UI, null, null, 0);
   end On_Row_Activated;

   --------------------
   -- On_Show_Syntax --
   --------------------

   procedure On_Show_Syntax
     (Widget : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      Entity : Komnenos.Entities.Entity_Reference)
   is
      pragma Unreferenced (Widget);
      use type Komnenos.Entities.Entity_Reference;
      Syntax_Entity : constant Komnenos.Entities.Entity_Reference :=
                        Komnenos.Entities.Source.Aquarius_Source.Syntax_Entity
                          (Current_UI, Entity);
   begin
      if Syntax_Entity /= null then
         Syntax_Entity.Select_Entity (Current_UI, null, null, 0);
      end if;
   end On_Show_Syntax;

   -------------------------------
   -- On_Tree_View_Button_Press --
   -------------------------------

   function On_Tree_View_Button_Press
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean
   is
      use Glib;
      Tree_View : constant Gtk.Tree_View.Gtk_Tree_View :=
                    Gtk.Tree_View.Gtk_Tree_View (Widget);
   begin
      if Event.Button = 3 then
         Ada.Text_IO.Put_Line ("show context menu");

         declare
            Selection : constant Gtk.Tree_Selection.Gtk_Tree_Selection :=
                          Tree_View.Get_Selection;
         begin
            if Selection.Count_Selected_Rows <= 1 then
               declare
                  Path : Gtk.Tree_Model.Gtk_Tree_Path;
                  Found : Boolean := False;
                  Cell_X, Cell_Y : Glib.Gint;
                  Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
                  pragma Unreferenced (Cell_X);
                  pragma Unreferenced (Cell_Y);
               begin
                  Tree_View.Get_Path_At_Pos
                    (X         => Gint (Event.X),
                     Y         => Gint (Event.Y),
                     Path      => Path,
                     Column    => Column,
                     Cell_X    => Cell_X,
                     Cell_Y    => Cell_Y,
                     Row_Found => Found);
                  if Found then
                     Selection.Unselect_All;
                     Selection.Select_Path (Path);
                  end if;
               end;
            end if;

         end;

         declare
            Entity : constant Komnenos.Entities.Entity_Reference :=
                       Get_Selected_Entity (Tree_View);
            References : constant Komnenos.Entities.Reference_Record_Array :=
                           Current_UI.References (Entity);

         begin
            Show_Context_Menu (Tree_View, Event,  Entity, References);
         end;

         return True;
      else
         return False;
      end if;
   end On_Tree_View_Button_Press;

   -----------------------
   -- Show_Context_Menu --
   -----------------------

   procedure Show_Context_Menu
     (Tree_View  : Gtk.Tree_View.Gtk_Tree_View;
      Event      : Gdk.Event.Gdk_Event_Button;
      Target     : Komnenos.Entities.Entity_Reference;
      References : Komnenos.Entities.Reference_Record_Array)
   is
      pragma Unreferenced (Tree_View);
   begin
      Gtk.Menu.Gtk_New (Popup_Menu);

      for I in References'Range loop
         declare
            Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
         begin
            Gtk.Menu_Item.Gtk_New_With_Label
              (Menu_Item, Current_UI.To_String (References (I)));
            Context_Menu_Reference_Callback.Connect
              (Menu_Item, Gtk.Menu_Item.Signal_Activate,
               Context_Menu_Reference_Callback.To_Marshaller
                 (On_Context_Menu_Item_Selected'Access),
               References (I));
            Popup_Menu.Append (Menu_Item);
         end;
      end loop;

      declare
         Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
      begin
         Gtk.Menu_Item.Gtk_New_With_Label
           (Menu_Item, "Show Syntax");
         Context_Menu_Entity_Callback.Connect
           (Menu_Item, Gtk.Menu_Item.Signal_Activate,
            Context_Menu_Entity_Callback.To_Marshaller
              (On_Show_Syntax'Access),
            Target);
         Popup_Menu.Append (Menu_Item);
      end;

      Popup_Menu.Show_All;
      Popup_Menu.Popup
        (Parent_Menu_Shell => null,
         Parent_Menu_Item  => null,
         Func              => null,
         Button            => Event.Button,
         Activate_Time     => Gtk.Main.Get_Current_Event_Time);

   end Show_Context_Menu;

   -------------------
   -- Show_Entities --
   -------------------

   procedure Show_Entities
     (Tree_View : Gtk.Tree_View.Gtk_Tree_View;
      Filter    : String)
   is
      use type Gtk.Tree_Model.Gtk_Tree_Model;
      Entity_Model : constant Gtk.List_Store.Gtk_List_Store :=
                       Gtk.List_Store.Gtk_List_Store (-Tree_View.Get_Model);

      procedure Show (Item : Komnenos.Entities.Entity_Reference);

      ----------
      -- Show --
      ----------

      procedure Show (Item : Komnenos.Entities.Entity_Reference) is
         Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
         Value : Glib.Values.GValue;
      begin
         Glib.Values.Init (Value, Glib.GType_Pointer);
         Glib.Values.Set_Address (Value, Item.all'Address);
         Entity_Model.Append (Iter);
         Entity_Model.Set (Iter, 0, Item.Display_Text);
         Entity_Model.Set_Value (Iter, 1, Value);
         Entity_Model.Set (Iter, 2, Item.Description);
      end Show;

   begin
      Entity_Model.Clear;
      Current_UI.Sort;
      Current_UI.Iterate (Filter, Show'Access);
   end Show_Entities;

end Komnenos.UI.Gtk_UI.Entity_Lists;
