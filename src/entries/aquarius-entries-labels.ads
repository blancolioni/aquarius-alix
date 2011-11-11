with Aquarius.Programs;

package Aquarius.Entries.Labels is

   function New_Label_Entry (Name        : String;
                             Declaration : Aquarius.Programs.Program_Tree)
                            return Table_Entry;

   function New_Incomplete_Label (Name      : String;
                                  Reference : Aquarius.Programs.Program_Tree)
                                 return Table_Entry;

   function Is_Label (Item : access Table_Entry_Record'Class) return Boolean;

end Aquarius.Entries.Labels;
