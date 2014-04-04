with Aquarius.Properties;               use Aquarius.Properties;
with Aquarius.UI;

package body Aquarius.Trees.Properties is

   -----------------
   -- Clear_Entry --
   -----------------

   procedure Clear_Entry
     (Tree  : in out Root_Tree_Type'Class)
   is
   begin
      Tree.Clear_Property (Entry_Property);
   end Clear_Entry;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry
     (Tree : Root_Tree_Type'Class)
     return Aquarius.Entries.Table_Entry
   is
   begin
      return Aquarius.Entries.Table_Entry (Tree.Property (Entry_Property));
   end Get_Entry;

   -----------------
   -- Get_Grammar --
   -----------------

   function Get_Grammar
     (Tree : Root_Tree_Type'Class)
     return Aquarius.Grammars.Aquarius_Grammar
   is
   begin
      return Aquarius.Grammars.Aquarius_Grammar
        (Tree.Property (Grammar_Property));
   end Get_Grammar;

   -----------------
   -- Get_Grammar --
   -----------------

   function Get_Grammar
     (Tree : not null access constant Root_Tree_Type'Class)
     return Aquarius.Grammars.Aquarius_Grammar
   is
   begin
      return Aquarius.Grammars.Aquarius_Grammar
        (Tree.Property (Grammar_Property));
   end Get_Grammar;

   --------------------
   -- Get_Interactor --
   --------------------

   function Get_Interactor
     (Tree : Root_Tree_Type'Class)
     return access Aquarius.Interaction.Interactor'Class
   is
   begin
      return Aquarius.Interaction.Interactor
        (Tree.Property (Interactor_Property).all)'Access;
   end Get_Interactor;

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project
     (Tree : Root_Tree_Type'Class)
     return Aquarius.Projects.Aquarius_Project
   is
   begin
      return Aquarius.Projects.Aquarius_Project
        (Tree.Property (Project_Property));
   end Get_Project;

   ----------------------
   -- Get_Symbol_Table --
   ----------------------

   function Get_Symbol_Table
     (Tree : Root_Tree_Type'Class)
     return Aquarius.Entries.Symbol_Table
   is
   begin
      return Aquarius.Entries.Symbol_Table
        (Tree.Property (Symbol_Table_Property));
   end Get_Symbol_Table;

   ----------------
   -- Get_Syntax --
   ----------------

   function Get_Syntax
     (Tree : Root_Tree_Type'Class)
     return Aquarius.Syntax.Syntax_Tree
   is
   begin
      return Aquarius.Syntax.Syntax_Tree (Tree.Property (Syntax_Property));
   end Get_Syntax;

   ----------------
   -- Get_Syntax --
   ----------------

   function Get_Syntax
     (Tree : not null access constant Root_Tree_Type'Class)
     return Aquarius.Syntax.Syntax_Tree
   is
   begin
      return Get_Syntax (Tree.all);
   end Get_Syntax;

   ----------------
   -- Get_Tagatha --
   ----------------

   function Get_Tagatha
     (Tree : Root_Tree_Type'Class)
     return Aquarius.Tagatha_Object.Aquarius_Tagatha_Object
   is
   begin
      return Aquarius.Tagatha_Object.Aquarius_Tagatha_Object
        (Tree.Property (Tagatha_Property));
   end Get_Tagatha;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Tree : Root_Tree_Type'Class)
     return Aquarius.Types.Aquarius_Type
   is
   begin
      return Aquarius.Types.Aquarius_Type (Tree.Property (Type_Property));
   end Get_Type;

   ------------
   -- Get_UI --
   ------------

   function Get_UI
     (Tree : Root_Tree_Type'Class)
     return Aquarius.UI.Aquarius_UI
   is
   begin
      return Aquarius.UI.Aquarius_UI (Tree.Property (UI_Property));
   end Get_UI;

   ---------------
   -- Has_Entry --
   ---------------

   function Has_Entry
     (Tree : Root_Tree_Type'Class)
     return Boolean
   is
   begin
      return Tree.Has_Property (Entry_Property);
   end Has_Entry;

   -----------------
   -- Has_Grammar --
   -----------------

   function Has_Grammar
     (Tree : Root_Tree_Type'Class)
     return Boolean
   is
   begin
      return Tree.Has_Property (Grammar_Property);
   end Has_Grammar;

   -----------------
   -- Has_Interactor --
   -----------------

   function Has_Interactor
     (Tree : Root_Tree_Type'Class)
     return Boolean
   is
   begin
      return Tree.Has_Property (Interactor_Property);
   end Has_Interactor;

   -----------------
   -- Has_Project --
   -----------------

   function Has_Project
     (Tree : Root_Tree_Type'Class)
     return Boolean
   is
   begin
      return Tree.Has_Property (Project_Property);
   end Has_Project;

   ----------------------
   -- Has_Symbol_Table --
   ----------------------

   function Has_Symbol_Table
     (Tree : Root_Tree_Type'Class)
     return Boolean
   is
   begin
      return Tree.Has_Property (Symbol_Table_Property);
   end Has_Symbol_Table;

   ----------------
   -- Has_Syntax --
   ----------------

   function Has_Syntax
     (Tree : Root_Tree_Type'Class)
     return Boolean
   is
   begin
      return Tree.Has_Property (Syntax_Property);
   end Has_Syntax;

   ----------------
   -- Has_Tagatha --
   ----------------

   function Has_Tagatha
     (Tree : Root_Tree_Type'Class)
     return Boolean
   is
   begin
      return Tree.Has_Property (Tagatha_Property);
   end Has_Tagatha;

   --------------
   -- Has_Type --
   --------------

   function Has_Type
     (Tree : Root_Tree_Type'Class)
     return Boolean
   is
   begin
      return Tree.Has_Property (Type_Property);
   end Has_Type;

   ------------
   -- Has_UI --
   ------------

   function Has_UI
     (Tree : Root_Tree_Type'Class)
     return Boolean
   is
   begin
      return Tree.Has_Property (UI_Property);
   end Has_UI;

   ---------------
   -- Set_Entry --
   ---------------

   procedure Set_Entry
     (Tree  : in out Root_Tree_Type'Class;
      Value : not null access Aquarius.Entries.Table_Entry_Record'Class)
   is
   begin
      Tree.Set_Property (Entry_Property, Value);
   end Set_Entry;

   -----------------
   -- Set_Grammar --
   -----------------

   procedure Set_Grammar
     (Tree  : in out Root_Tree_Type'Class;
      Value : Aquarius.Grammars.Aquarius_Grammar)
   is
   begin
      Tree.Set_Property (Grammar_Property, Value);
      Tree.Set_Property (Pool_Property, Value);
   end Set_Grammar;

   --------------------
   -- Set_Interactor --
   --------------------

   procedure Set_Interactor
     (Tree  : in out Root_Tree_Type'Class;
      Value : not null access Aquarius.Interaction.Interactor'Class)
   is
   begin
      Tree.Set_Property (Interactor_Property, Value);
      Tree.Set_Property (Pool_Property, Value);
   end Set_Interactor;

   -----------------
   -- Set_Project --
   -----------------

   procedure Set_Project
     (Tree  : in out Root_Tree_Type'Class;
      Value : Aquarius.Projects.Aquarius_Project)
   is
   begin
      Tree.Set_Property (Project_Property, Value);
   end Set_Project;

   ----------------------
   -- Set_Symbol_Table --
   ----------------------

   procedure Set_Symbol_Table
     (Tree : in out Root_Tree_Type'Class;
      Value : Aquarius.Entries.Symbol_Table)
   is
   begin
      Tree.Set_Property (Symbol_Table_Property, Value);
   end Set_Symbol_Table;

   ----------------
   -- Set_Syntax --
   ----------------

   procedure Set_Syntax
     (Tree : in out Root_Tree_Type'Class;
      Value : Aquarius.Syntax.Syntax_Tree)
   is
   begin
      Tree.Set_Property (Syntax_Property, Value);
   end Set_Syntax;

   ----------------
   -- Set_Syntax --
   ----------------

   procedure Set_Syntax
     (Tree : not null access Root_Tree_Type'Class;
      Value : Aquarius.Syntax.Syntax_Tree)
   is
   begin
      Set_Syntax (Tree.all, Value);
   end Set_Syntax;

   -----------------
   -- Set_Tagatha --
   -----------------

   procedure Set_Tagatha
     (Tree : in out Root_Tree_Type'Class;
      Value : Aquarius.Tagatha_Object.Aquarius_Tagatha_Object)
   is
   begin
      Tree.Set_Property (Tagatha_Property, Value);
   end Set_Tagatha;

   ---------------
   -- Set_Type --
   ---------------

   procedure Set_Type
     (Tree : in out Root_Tree_Type'Class;
      Value : not null access Aquarius.Types.Root_Aquarius_Type'Class)
   is
   begin
      Tree.Set_Property (Type_Property,
                         Aquarius.Types.Aquarius_Type (Value));
   end Set_Type;

   ------------
   -- Set_UI --
   ------------

   procedure Set_UI
     (Tree  : in out Root_Tree_Type'Class;
      Value : Aquarius.UI.Aquarius_UI)
   is
   begin
      Set_Property (Tree, UI_Property, Value);
   end Set_UI;

end Aquarius.Trees.Properties;
