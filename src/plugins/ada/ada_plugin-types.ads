with Aquarius.Entries;
with Aquarius.Types;

package Ada_Plugin.Types is

   function New_Enumeration_Type
     return Aquarius.Types.Aquarius_Type;

   function New_Literal (For_Enumeration : Aquarius.Types.Aquarius_Type;
                         Display_Name    : String)
                        return Aquarius.Entries.Table_Entry;

   function New_Literal (For_Enumeration : Aquarius.Types.Aquarius_Type;
                         Value           : Character)
                        return Aquarius.Entries.Table_Entry;

end Ada_Plugin.Types;
