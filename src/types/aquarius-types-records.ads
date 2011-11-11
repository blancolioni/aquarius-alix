with Aquarius.Entries;

package Aquarius.Types.Records is

   function New_Record_Type return Aquarius_Type;

   function Is_Record_Type (Item : Aquarius_Type) return Boolean;

   procedure Add_Component (To_Record : Aquarius_Type;
                            Component : Aquarius.Entries.Table_Entry);

   function Get_Component (From_Record : Aquarius_Type;
                           Name        : String)
                          return Aquarius.Entries.Table_Entry;

end Aquarius.Types.Records;
