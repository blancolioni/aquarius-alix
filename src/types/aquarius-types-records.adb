with Aquarius.Types.Composite;

package body Aquarius.Types.Records is

   type Record_Type is new Aquarius.Types.Composite.Root_Composite_Type with
      record
         Components : Aquarius.Entries.Symbol_Table;
      end record;

   overriding
   function Create_Derived_Type
     (Item : Record_Type) return Aquarius_Type;
   overriding
   function Description (Item : Record_Type) return String;

   -------------------
   -- Add_Component --
   -------------------

   procedure Add_Component (To_Record : Aquarius_Type;
                            Component : Aquarius.Entries.Table_Entry)
   is
   begin
      Record_Type (To_Record.all).Components.Insert (Component);
   end Add_Component;

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   overriding
   function Create_Derived_Type
     (Item : Record_Type)
     return Aquarius_Type
   is
   begin
      return new Record_Type'(Item);
   end Create_Derived_Type;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description (Item : Record_Type) return String is
      pragma Unreferenced (Item);
   begin
      return "a record type";
   end Description;

   -------------------
   -- Get_Component --
   -------------------

   function Get_Component (From_Record : Aquarius_Type;
                           Name        : String)
                          return Aquarius.Entries.Table_Entry
   is
      use type Aquarius.Entries.Symbol_Table;
   begin
      if From_Record = null then
         raise Constraint_Error with
           "attempt to retrieve component " & Name &
           " from null record type";
      end if;
      if Record_Type (From_Record.all).Components = null then
         raise Constraint_Error with
           "attempt to retrieve component " & Name &
           " from record with no component table";
      end if;

      return Record_Type (From_Record.all).Components.Retrieve (Name);
   end Get_Component;

   --------------------
   -- Is_Record_Type --
   --------------------

   function Is_Record_Type (Item : Aquarius_Type) return Boolean is
   begin
      return Item.all in Record_Type'Class;
   end Is_Record_Type;

   ---------------------
   -- New_Record_Type --
   ---------------------

   function New_Record_Type return Aquarius_Type is
      Result : Record_Type;
   begin
      Result.Components :=
        Aquarius.Entries.New_Symbol_Table ("record components");
      return new Record_Type'(Result);
   end New_Record_Type;

end Aquarius.Types.Records;
