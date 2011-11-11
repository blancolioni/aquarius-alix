package body Aquarius.Entries.Packages is

   type Package_Entry_Record is new Table_Entry_Record with
      record
         Table          : Symbol_Table;
      end record;

   type Package_Reference_Record is new Table_Entry_Record with
      record
         Reference      : Table_Entry;
         Table          : Symbol_Table;
         Child_Packages : Symbol_Table;
      end record;

   -----------------------
   -- Add_Child_Package --
   -----------------------

   procedure Add_Child_Package (Item  : Table_Entry;
                                Child : Table_Entry)
   is
   begin
      Package_Reference_Record (Item.all).Child_Packages.Insert (Child);
   end Add_Child_Package;

   -----------------------
   -- Get_Child_Package --
   -----------------------

   function Get_Child_Package (Item : Table_Entry;
                               Name : String)
                              return Table_Entry
   is
   begin
      return Package_Reference_Record (Item.all).Child_Packages.Retrieve
        (Name);
   end Get_Child_Package;

   ----------------------
   -- Get_Symbol_Table --
   ----------------------

   function Get_Symbol_Table (Item : Table_Entry)
                             return Symbol_Table
   is
   begin
      if Item.all in Package_Entry_Record'Class then
         return Package_Entry_Record (Item.all).Table;
      else
         return Package_Reference_Record (Item.all).Table;
      end if;
   end Get_Symbol_Table;

   ----------------
   -- Is_Package --
   ----------------

   function Is_Package (Item : Table_Entry) return Boolean is
   begin
      return Item.all in Package_Entry_Record'Class;
   end Is_Package;

   --------------------------
   -- Is_Package_Reference --
   --------------------------

   function Is_Package_Reference (Item : Table_Entry) return Boolean is
   begin
      return Item.all in Package_Reference_Record'Class;
   end Is_Package_Reference;

   ------------------
   -- Make_Visible --
   ------------------

   procedure Make_Visible (Item  : Table_Entry) is
      Table : constant Symbol_Table :=
        Package_Reference_Record (Item.all).Table;
   begin
      Insert_Table (Table, Item.Owner);
   end Make_Visible;

   -----------------------
   -- New_Package_Entry --
   -----------------------

   function New_Package_Entry
     (Name        : in String;
      Parent      : in Table_Entry;
      Declaration : in Aquarius.Programs.Program_Tree;
      Table       : in Symbol_Table)
      return Table_Entry
   is
      Result : Package_Entry_Record;
   begin
      Result.Create_Entry (Name, Declaration);
      Result.Table := Table;
      Result.Entry_Owner := Parent;
      return new Package_Entry_Record'(Result);
   end New_Package_Entry;

   ---------------------------
   -- New_Package_Reference --
   ---------------------------

   function New_Package_Reference
     (Declaration : access Aquarius.Trees.Root_Tree_Type'Class;
      Reference   : in Table_Entry)
     return Table_Entry
   is
      Result : Package_Reference_Record;
   begin
      Result.Create_Entry (Name (Reference.all), Declaration);
      Result.Reference := Reference;
      Result.Table := Package_Entry_Record (Reference.all).Table;
      Result.Child_Packages := New_Symbol_Table (Name (Reference.all));
      return new Package_Reference_Record'(Result);
   end New_Package_Reference;

   --------------------------
   -- New_Standard_Package --
   --------------------------

   function New_Standard_Package
     (Name           : in     String;
      Table          : in     Symbol_Table)
     return Table_Entry
   is
      Result : Package_Entry_Record;
   begin
      Result.Table := Table;
      Create_Entry (Result, Name, null);
      return new Package_Entry_Record'(Result);
   end New_Standard_Package;

end Aquarius.Entries.Packages;
