with Aquarius.Entries;
with Aquarius.Grammars;
with Aquarius.Interaction;
with Aquarius.Projects;
with Aquarius.Syntax;
with Aquarius.Tagatha_Object;
with Aquarius.Types;

limited with Aquarius.UI;

package Aquarius.Trees.Properties is

   function Get_Entry
     (Tree : Root_Tree_Type'Class)
     return Aquarius.Entries.Table_Entry;

   function Get_Grammar
     (Tree : Root_Tree_Type'Class)
     return Aquarius.Grammars.Aquarius_Grammar;

   function Get_Grammar
     (Tree : not null access constant Root_Tree_Type'Class)
     return Aquarius.Grammars.Aquarius_Grammar;

   function Get_Interactor
     (Tree : Root_Tree_Type'Class)
     return access Aquarius.Interaction.Interactor'Class;

   function Get_Project
     (Tree : Root_Tree_Type'Class)
     return Aquarius.Projects.Aquarius_Project;

   function Get_Symbol_Table
     (Tree : Root_Tree_Type'Class)
     return Aquarius.Entries.Symbol_Table;

   function Get_Syntax
     (Tree : Root_Tree_Type'Class)
     return Aquarius.Syntax.Syntax_Tree;

   function Get_Syntax
     (Tree : not null access constant Root_Tree_Type'Class)
     return Aquarius.Syntax.Syntax_Tree;

   function Get_Type
     (Tree : Root_Tree_Type'Class)
     return Aquarius.Types.Aquarius_Type;

   function Get_Tagatha
     (Tree : Root_Tree_Type'Class)
     return Aquarius.Tagatha_Object.Aquarius_Tagatha_Object;

   function Get_UI
     (Tree : Root_Tree_Type'Class)
     return access Aquarius.UI.Aquarius_UI'Class;

   function Has_Entry
     (Tree : Root_Tree_Type'Class)
     return Boolean;

   function Has_Grammar
     (Tree : Root_Tree_Type'Class)
     return Boolean;

   function Has_Interactor
     (Tree : Root_Tree_Type'Class)
     return Boolean;

   function Has_Project
     (Tree : Root_Tree_Type'Class)
     return Boolean;

   function Has_Symbol_Table
     (Tree : Root_Tree_Type'Class)
     return Boolean;

   function Has_Syntax
     (Tree : Root_Tree_Type'Class)
     return Boolean;

   function Has_Tagatha
     (Tree : Root_Tree_Type'Class)
     return Boolean;

   function Has_Type
     (Tree : Root_Tree_Type'Class)
     return Boolean;

   function Has_UI
     (Tree : Root_Tree_Type'Class)
     return Boolean;

   procedure Set_Entry
     (Tree  : in out Root_Tree_Type'Class;
      Value : not null access Aquarius.Entries.Table_Entry_Record'Class);

   procedure Set_Grammar
     (Tree  : in out Root_Tree_Type'Class;
      Value : Aquarius.Grammars.Aquarius_Grammar);

   procedure Set_Interactor
     (Tree  : in out Root_Tree_Type'Class;
      Value : not null access Aquarius.Interaction.Interactor'Class);

   procedure Set_Project
     (Tree  : in out Root_Tree_Type'Class;
      Value : Aquarius.Projects.Aquarius_Project);

   procedure Set_Symbol_Table
     (Tree : in out Root_Tree_Type'Class;
      Value : Aquarius.Entries.Symbol_Table);

   procedure Set_Syntax
     (Tree : in out Root_Tree_Type'Class;
      Value : Aquarius.Syntax.Syntax_Tree);

   procedure Set_Syntax
     (Tree : not null access Root_Tree_Type'Class;
      Value : Aquarius.Syntax.Syntax_Tree);

   procedure Set_Tagatha
     (Tree : in out Root_Tree_Type'Class;
      Value : Aquarius.Tagatha_Object.Aquarius_Tagatha_Object);

   procedure Set_Type
     (Tree : in out Root_Tree_Type'Class;
      Value : not null access Aquarius.Types.Root_Aquarius_Type'Class);

   procedure Set_UI
     (Tree  : in out Root_Tree_Type'Class;
      Value : not null access Aquarius.UI.Aquarius_UI'Class);

   procedure Clear_Entry
     (Tree  : in out Root_Tree_Type'Class);

end Aquarius.Trees.Properties;
