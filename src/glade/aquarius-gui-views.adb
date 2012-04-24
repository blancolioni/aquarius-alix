with System.Address_To_Access_Conversions;

with Glib;

with Gtk.Cell_Renderer_Text;
with Gtk.Enums;
with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);

with Gtk.Label;
with Gtk.Notebook;
with Gtk.Scrolled_Window;
with Gtk.Tree_Model;
with Gtk.Tree_Selection;
with Gtk.Tree_Store;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;

with Aquarius.Buffers;
with Aquarius.Programs;
with Aquarius.Source;
with Aquarius.Trees;

with Aquarius.GUI.Manager;

with Aquarius.Fragments.Programs;

package body Aquarius.GUI.Views is

   package Select_Item_Handler is
      new Gtk.Handlers.Callback (Gtk.Tree_View.Gtk_Tree_View_Record);

   package Project_Tree_To_Access is
     new System.Address_To_Access_Conversions
       (Aquarius.Projects.Root_Project_Tree'Class);

   View_Book        : Gtk.Notebook.Gtk_Notebook;

   procedure Row_Activated_Callback
     (Widget : access Gtk.Tree_View.Gtk_Tree_View_Record'Class);

   procedure Add_View
     (View : access Aquarius.Projects.Root_Project_View'Class);

   procedure Add_Tree
     (Store  : Gtk.Tree_Store.Gtk_Tree_Store;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      Tree   : Aquarius.Trees.Tree);

   function Add_Node
     (Store  : Gtk.Tree_Store.Gtk_Tree_Store;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node   : Aquarius.Trees.Tree)
     return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Get_Program_From_Node
     (Node : Aquarius.Projects.Project_Tree)
      return Aquarius.Programs.Program_Tree;

   --------------
   -- Add_Node --
   --------------

   function Add_Node
     (Store  : Gtk.Tree_Store.Gtk_Tree_Store;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node   : Aquarius.Trees.Tree)
     return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Result : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Store.Append (Result, Parent);
      Store.Set (Result, 0, Node.Text);
      Store.Set (Result, 1, Node.all'Address);
      return Result;
   end Add_Node;

   --------------
   -- Add_Tree --
   --------------

   procedure Add_Tree
     (Store  : Gtk.Tree_Store.Gtk_Tree_Store;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      Tree   : Aquarius.Trees.Tree)
   is
      use Gtk.Tree_Store;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Iter := Add_Node (Store, Parent, Tree);
      for I in 1 .. Tree.Child_Count loop
         Add_Tree (Store, Iter, Tree.Child (I));
      end loop;
   end Add_Tree;

   --------------
   -- Add_View --
   --------------

   procedure Add_View
     (View : access Aquarius.Projects.Root_Project_View'Class)
   is
      Page_Label  : Gtk.Label.Gtk_Label;
      Scrolled    : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Tree_View   : Gtk.Tree_View.Gtk_Tree_View;
      Model       : Gtk.Tree_Store.Gtk_Tree_Store;
      Text_Render : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Text_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Num         : Glib.Gint;
      pragma Warnings (Off, Num);
   begin
      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy (Gtk.Enums.Policy_Automatic,
                           Gtk.Enums.Policy_Automatic);
      Gtk.Label.Gtk_New (Page_Label, View.Name);
      Gtk.Tree_Store.Gtk_New (Model,
        (0     => Glib.GType_String,
         1     => Glib.GType_Pointer));
      Gtk.Tree_View.Gtk_New (Tree_View, Model);
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Render);
      Gtk.Tree_View_Column.Gtk_New (Text_Column);
      Num := Tree_View.Append_Column (Text_Column);
      Text_Column.Pack_Start (Text_Render, True);
      Text_Column.Set_Sizing (Gtk.Tree_View_Column.Tree_View_Column_Autosize);
      Text_Column.Add_Attribute (Text_Render, "text", 0);

      Add_Tree (Model, Gtk.Tree_Model.Null_Iter, View.Contents);

      Scrolled.Add (Tree_View);
      Scrolled.Show_All;

      View_Book.Append_Page (Scrolled, Page_Label);

      Select_Item_Handler.Connect
        (Tree_View, "row-activated",
         Row_Activated_Callback'Access);

   end Add_View;

   ---------------------------
   -- Get_Program_From_Node --
   ---------------------------

   function Get_Program_From_Node
     (Node : Aquarius.Projects.Project_Tree)
      return Aquarius.Programs.Program_Tree
   is
      use Aquarius.Programs;
      Target : constant Program_Tree := Node.Target;
   begin
      if Target = null then
         declare
            Buffer : constant Aquarius.Buffers.Aquarius_Buffer :=
                       Current_Project.Get_Buffer (Node.Text, True);
         begin
            return Buffer.Program;
         end;
      else
         return Target;
      end if;
   end Get_Program_From_Node;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (Builder : Gtk.Builder.Gtk_Builder) is
   begin
      View_Book :=
        Gtk.Notebook.Gtk_Notebook
        (Builder.Get_Object ("View_Book"));
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
--        Page        : constant Positive :=
--          Natural (View_Book.Get_Current_Page) + 1;
   begin
      Gtk.Tree_Selection.Get_Selected (Selection, Model, Iter);
      if not Model.Has_Child (Iter) then
         declare
            Node_Addr : constant System.Address :=
                          Model.Get_Address (Iter, 1);
            Node      : constant Aquarius.Projects.Project_Tree :=
                          Aquarius.Projects.Project_Tree
                            (Project_Tree_To_Access.To_Pointer
                               (Node_Addr));
            Program   : constant Aquarius.Programs.Program_Tree :=
                          Get_Program_From_Node (Node);
            Location  : constant Aquarius.Source.Source_Position :=
                          Program.Get_Location;
            Buffer    : constant Aquarius.Buffers.Aquarius_Buffer :=
                          Current_Project.Get_Buffer
                            (Aquarius.Source.Get_File_Name
                               (Aquarius.Source.Get_Source_File
                                  (Location)),
                             False);
            Fragment  : constant Aquarius.Fragments.Aquarius_Fragment :=
                          Aquarius.Fragments.Programs.Create_Program
                            (300, 300, Buffer.Program);
         begin
            Aquarius.GUI.Manager.Add_Fragment
              (Fragment, 20, 20);
         end;
      end if;
   end Row_Activated_Callback;

   ------------------
   -- Update_Views --
   ------------------

   procedure Update_Views (Project : Aquarius.Projects.Aquarius_Project) is
      use type Glib.Gint;
   begin

      while View_Book.Get_N_Pages > 0 loop
         View_Book.Remove_Page (0);
      end loop;

      for I in 1 .. Project.View_Count loop
         Project.View (I).Reload;
         Add_View (Project.View (I));
      end loop;
   end Update_Views;

end Aquarius.GUI.Views;
