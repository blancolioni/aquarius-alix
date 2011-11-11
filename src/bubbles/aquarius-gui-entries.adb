with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

with Glib;

with Gtk.Cell_Renderer_Text;
with Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Tree_Model;
with Gtk.Tree_Selection;
with Gtk.Tree_Store;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;

with Aquarius.Bubbles.Buffers;
with Aquarius.Buffers;
with Aquarius.Entries;
with Aquarius.Programs;
with Aquarius.Source;

package body Aquarius.GUI.Entries is

   package Select_Item_Handler is
      new Gtk.Handlers.Callback (Gtk.Tree_View.Gtk_Tree_View_Record);

   package Entry_Changed_Handler is
      new Gtk.Handlers.Callback (Gtk.GEntry.Gtk_Entry_Record);

   Entry_Tree_View  : Gtk.Tree_View.Gtk_Tree_View;
   Text_Render      : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
   Text_Column      : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   Filter           : Gtk.GEntry.Gtk_Entry;
   Boxed_Buffer     : Glib.GType;
   Current_Project  : Aquarius.Projects.Aquarius_Project;

   type Entry_Element_Record is
      record
         Item : Aquarius.Entries.Table_Entry;
      end record;

   package Boxed_Entry_Element is
      new System.Address_To_Access_Conversions (Entry_Element_Record);

   function Copy_Entry_Element (Boxed : System.Address) return System.Address;
   pragma Convention (C, Copy_Entry_Element);

   procedure Free_Entry_Element (Boxed : System.Address);
   pragma Convention (C, Free_Entry_Element);

   procedure Row_Activated_Callback
     (Widget : access Gtk.Tree_View.Gtk_Tree_View_Record'Class);

   procedure Filter_Changed_Callback
     (Widget : access Gtk.GEntry.Gtk_Entry_Record'Class);

   procedure Add_Entries
     (Filter  : in String);

   procedure Add_Node
     (Model     : Gtk.Tree_Store.Gtk_Tree_Store;
      New_Entry : Aquarius.Entries.Table_Entry);

   procedure Update_Entries;

   -----------------
   -- Add_Entries --
   -----------------

   procedure Add_Entries (Filter  : in String)
   is
      use Aquarius.Entries;
      Model       : constant Gtk.Tree_Store.Gtk_Tree_Store :=
        Gtk.Tree_Store.Gtk_Tree_Store (Entry_Tree_View.Get_Model);
      Entries     : constant Array_Of_Entries :=
        Aquarius.Projects.Filter_Entries (Current_Project, Filter);
   begin

      for I in Entries'Range loop
         Add_Node (Model, Entries (I));
      end loop;

   end Add_Entries;

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node
     (Model     : Gtk.Tree_Store.Gtk_Tree_Store;
      New_Entry : Aquarius.Entries.Table_Entry)
   is
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Elem : constant Boxed_Entry_Element.Object_Pointer :=
        new Entry_Element_Record'(Item => New_Entry);
   begin
      Model.Append (Iter, Gtk.Tree_Model.Null_Iter);
      Model.Set (Iter, 0, New_Entry.Display_Name);
      Model.Set (Iter, 1, Boxed_Entry_Element.To_Address (Elem));
   end Add_Node;

   -----------------------
   -- Copy_Entry_Element --
   -----------------------

   function Copy_Entry_Element (Boxed : System.Address)
                               return System.Address
   is
      Result : constant Boxed_Entry_Element.Object_Pointer :=
        new Entry_Element_Record'(Boxed_Entry_Element.To_Pointer (Boxed).all);
   begin
      return Boxed_Entry_Element.To_Address (Result);
   end Copy_Entry_Element;

   ----------------------------
   -- Entry_Changed_Callback --
   ----------------------------

   procedure Filter_Changed_Callback
     (Widget : access Gtk.GEntry.Gtk_Entry_Record'Class)
   is
      pragma Unreferenced (Widget);
   begin
      Update_Entries;
   end Filter_Changed_Callback;

   -----------------------
   -- Free_Entry_Element --
   -----------------------

   procedure Free_Entry_Element (Boxed : System.Address) is

      procedure Free is
         new Ada.Unchecked_Deallocation (Entry_Element_Record,
                                         Boxed_Entry_Element.Object_Pointer);

      Item : Boxed_Entry_Element.Object_Pointer :=
        Boxed_Entry_Element.To_Pointer (Boxed);
   begin
      Free (Item);
   end Free_Entry_Element;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (XML : Glade.XML.Glade_XML) is
      Num         : Glib.Gint;
      pragma Warnings (Off, Num);
   begin
      Boxed_Buffer :=
        Glib.Boxed_Type_Register_Static ("aquarius_boxed_buffer",
                                         Copy_Entry_Element'Access,
                                         Free_Entry_Element'Access);
      Entry_Tree_View :=
        Gtk.Tree_View.Gtk_Tree_View
        (Glade.XML.Get_Widget (XML, "Entry_List"));

      Gtk.Cell_Renderer_Text.Gtk_New (Text_Render);
      Gtk.Tree_View_Column.Gtk_New (Text_Column);
      Num := Entry_Tree_View.Append_Column (Text_Column);
      Text_Column.Pack_Start (Text_Render, True);
      Text_Column.Set_Sizing (Gtk.Tree_View_Column.Tree_View_Column_Autosize);
      Text_Column.Add_Attribute (Text_Render, "text", 0);

      Select_Item_Handler.Connect
        (Entry_Tree_View, "row-activated",
         Row_Activated_Callback'Access);

      Filter :=
        Gtk.GEntry.Gtk_Entry
        (Glade.XML.Get_Widget (XML, "Entry_Search"));

      Entry_Changed_Handler.Connect
        (Filter, "changed", Filter_Changed_Callback'Access);

   end Initialise;

   ----------------------------
   -- Row_Activated_Callback --
   ----------------------------

   procedure Row_Activated_Callback
     (Widget : access Gtk.Tree_View.Gtk_Tree_View_Record'Class)
   is
      Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Selection : constant Gtk.Tree_Selection.Gtk_Tree_Selection :=
        Widget.Get_Selection;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Gtk.Tree_Selection.Get_Selected (Selection, Model, Iter);

      declare
         use type Boxed_Entry_Element.Object_Pointer;
         use type Aquarius.Entries.Table_Entry;
         Addr   : constant System.Address := Model.Get_Address (Iter, 1);
         Elem   : constant Boxed_Entry_Element.Object_Pointer :=
           Boxed_Entry_Element.To_Pointer (Addr);
      begin
         if Elem = null or else
           Elem.Item = null
         then
            Ada.Text_IO.Put_Line ("no selection");
         else
            declare
               use Aquarius.Buffers, Aquarius.Source;
               File_Name  : constant String :=
                 Get_File_Name
                 (Get_Source_File (Elem.Item.Declaration.Get_Location));
               Parent_Buffer : constant Aquarius_Buffer :=
                 Current_Project.Get_Buffer (File_Name);
               New_Buffer : constant Aquarius_Buffer :=
                 New_Child_Buffer (Parent_Buffer,
                                   Aquarius.Programs.Program_Tree
                                     (Elem.Item.Declaration));
               New_Bubble    : constant Aquarius.Bubbles.Aquarius_Bubble :=
                 Aquarius.Bubbles.Buffers.Create_Buffer_Bubble (New_Buffer);
            begin
               Create_New_Bubble (New_Bubble);
            end;
         end if;
      end;
   end Row_Activated_Callback;

   --          Elem.Node.Target = null
   --        then

   --              declare
   --                 Buffer : constant Aquarius.Buffers.Aquarius_Buffer :=
   --                   Current_Project.Get_Buffer
   --                         (Model.Get_String (Iter, 0));
   --              begin
   --                 Aquarius.GUI.Source.Load_Buffer (Buffer);
   --              end;
   --           else
   --              declare
   --                 use type Aquarius.Trees.Tree;
   --                 Target : constant Aquarius.Programs.Program_Tree :=
   --                   Elem.Node.Target;
   --                 Buffer : constant Aquarius.Buffers.Aquarius_Buffer :=
   --                   Current_Project.Get_Buffer
   --                   (Aquarius.Source.Get_File_Name
   --                      (Aquarius.Source.Get_Source_File
   --                         (Target.Get_Location)));
   --              begin
   --                 Aquarius.GUI.Source.Load_Buffer
   --                   (Buffer  => Buffer,
   --                    Start   => Target);
   --              end;
   --           end if;
   --        end;
   --     end if;
   --  end Row_Activated_Callback;

   -----------------
   -- Set_Project --
   -----------------

   procedure Set_Project (Project : Aquarius.Projects.Aquarius_Project) is
   begin
      Current_Project := Project;
      Update_Entries;
   end Set_Project;

   ------------------
   -- Update_Entries --
   ------------------

   procedure Update_Entries is
      Model : Gtk.Tree_Store.Gtk_Tree_Store;
      Filter_Text : constant String :=
        Filter.Get_Text;
   begin
      Gtk.Tree_Store.Gtk_New (Model,
                              (0   => Glib.GType_String,
                               1   => Boxed_Buffer));
      Entry_Tree_View.Set_Model (Gtk.Tree_Model.Gtk_Tree_Model (Model));
      Add_Entries (Filter_Text);
   end Update_Entries;

end Aquarius.GUI.Entries;
