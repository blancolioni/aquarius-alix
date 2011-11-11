package body Aquarius.Entries.Labels is

   type Label_Entry_Record is new Table_Entry_Record with null record;

   --------------
   -- Is_Label --
   --------------

   function Is_Label (Item : access Table_Entry_Record'Class)
                     return Boolean
   is
   begin
      return Item.all in Label_Entry_Record'Class;
   end Is_Label;

   --------------------------
   -- New_Incomplete_Label --
   --------------------------

   function New_Incomplete_Label
     (Name      : in String;
      Reference : in Aquarius.Programs.Program_Tree)
     return Table_Entry
   is
      Label  : Label_Entry_Record;
      Result : Table_Entry;
   begin
      Create_Entry (Label, Name, null);
      Result := new Label_Entry_Record'(Label);
      Set_Complete (Result, False);
      Add_Reference (Result, Reference);
      return Result;
   end New_Incomplete_Label;

   ---------------------
   -- New_Label_Entry --
   ---------------------

   function New_Label_Entry
     (Name        : in String;
      Declaration : in Aquarius.Programs.Program_Tree)
     return Table_Entry
   is
      Result : Label_Entry_Record;
   begin
      Create_Entry (Result, Name, Declaration);
      return new Label_Entry_Record'(Result);
   end New_Label_Entry;

end Aquarius.Entries.Labels;
