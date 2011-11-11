with Aquarius.Types;
with Aquarius.Values;

with Tagatha.Fragments;

package Aquarius.Entries.Objects is

   function New_Object_Entry
     (Name           : in     String;
      Declaration    : access Aquarius.Trees.Root_Tree_Type'Class;
      Entry_Type     : not null access
                             Aquarius.Types.Root_Aquarius_Type'Class;
      Entry_Value    : in     Aquarius.Values.Aquarius_Value;
      Constant_Value : in     Boolean)
     return Table_Entry;

   function New_Tagatha_Entry
     (Name           : in     String;
      Declaration    : access Aquarius.Trees.Root_Tree_Type'Class;
      Entry_Type     : not null access
                             Aquarius.Types.Root_Aquarius_Type'Class;
      Entry_Fragment : in     Tagatha.Fragments.Tagatha_Fragment)
     return Table_Entry;

   function Is_Object_Entry
     (Item : access Table_Entry_Record'Class) return Boolean;

   function Is_Constant (Item : access Table_Entry_Record'Class)
                        return Boolean;

   function Is_Intrinsic (Item : access Table_Entry_Record'Class)
                         return Boolean;

   function Is_Stack_Object (Item : access Table_Entry_Record'Class)
                            return Boolean;

   function Object_Entry_Type
     (Item : access Table_Entry_Record'Class)
     return Aquarius.Types.Aquarius_Type;

   function Object_Entry_Default_Value
     (Item : access Table_Entry_Record'Class)
     return Aquarius.Values.Aquarius_Value;

   function Object_Entry_Intrinsic_Value
     (Item : access Table_Entry_Record'Class)
     return Tagatha.Fragments.Tagatha_Fragment;

   procedure Set_Frame_Offset (Item   : access Table_Entry_Record'Class;
                               Offset : in     Integer);
   function Frame_Offset (Item : access Table_Entry_Record'Class)
                         return Integer;

   function Object_Entry_Constraint return Entry_Constraint'Class;
   function Object_Entry_Constraint (Name : String)
                                    return Entry_Constraint'Class;

end Aquarius.Entries.Objects;
