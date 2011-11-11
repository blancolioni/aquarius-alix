with Aquarius.Programs;
with Aquarius.Trees;

package Aquarius.Entries.Packages is

   pragma Elaborate_Body (Aquarius.Entries.Packages);

   --  A 'package' is a collection of other entries.  A package
   --  can be owned by another package.

   function New_Package_Entry
     (Name        : in String;
      Parent      : in Table_Entry;
      Declaration : in Aquarius.Programs.Program_Tree;
      Table       : in Symbol_Table)
      return Table_Entry;

   --  A 'standard' package has a built-in symbol table
   function New_Standard_Package
     (Name           : in     String;
      Table          : in     Symbol_Table)
     return Table_Entry;

   --  We can reference a package

   function New_Package_Reference
     (Declaration : access Aquarius.Trees.Root_Tree_Type'Class;
      Reference   : in Table_Entry)
     return Table_Entry;

   --  Make_Visible: make all entries in the given package
   --  visible in the symbol table
   procedure Make_Visible (Item  : Table_Entry);

   function Is_Package (Item : Table_Entry) return Boolean;
   function Is_Package_Reference (Item : Table_Entry) return Boolean;

   function Get_Symbol_Table (Item : Table_Entry)
                             return Symbol_Table;

   function Get_Child_Package (Item : Table_Entry;
                               Name : String)
                              return Table_Entry;

   procedure Add_Child_Package (Item  : Table_Entry;
                                Child : Table_Entry);

end Aquarius.Entries.Packages;
