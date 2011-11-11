with Aquarius.Types;

package Aquarius.Entries.Types is

   pragma Elaborate_Body;

   function New_Type_Entry
     (Name        : in     String;
      Declaration : access Aquarius.Trees.Root_Tree_Type'Class;
      Entry_Type  : access Aquarius.Types.Root_Aquarius_Type'Class)
     return Table_Entry;

   function New_Type_Entry
     (Name        : in     String;
      Declaration : access Aquarius.Trees.Root_Tree_Type'Class)
     return Table_Entry;

   function Is_Type_Entry
     (Item : Table_Entry)
     return Boolean;

   function Get_Type (Item : Table_Entry) return Aquarius.Types.Aquarius_Type;
   procedure Complete_Type (Item      : in Table_Entry;
                            Full_Type : in Aquarius.Types.Aquarius_Type);

   function Is_Complete_Type (Item : Table_Entry) return Boolean;

end Aquarius.Entries.Types;
