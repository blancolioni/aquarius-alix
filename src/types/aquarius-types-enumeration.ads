with Aquarius.Entries;
with Aquarius.Trees;

package Aquarius.Types.Enumeration is

   function New_Enumeration_Type return Aquarius_Type;

   function New_Enumeration_Literal
     (Enumeration : Aquarius_Type;
      Declaration : access Aquarius.Trees.Root_Tree_Type'Class;
      Name        : String)
     return Aquarius.Entries.Table_Entry;

   procedure New_Enumeration_Literal
     (Enumeration : in Aquarius_Type;
      Item        : in Character);

   function Get_Literal (From_Enumeration : Aquarius_Type;
                         Name             : String)
                        return Aquarius.Entries.Table_Entry;

end Aquarius.Types.Enumeration;
