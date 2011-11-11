package body Aquarius.Entries.Objects is

   type Object_Entry_Record is new Table_Entry_Record with
      record
         Constant_Value : Boolean;
         Intrinsic      : Boolean    := False;
         Entry_Type     : Aquarius.Types.Aquarius_Type;
         Entry_Value    : Aquarius.Values.Aquarius_Value;
         Entry_Fragment : Tagatha.Fragments.Tagatha_Fragment;
         Offset         : Integer;
      end record;

   type Object_Entry is access all Object_Entry_Record'Class;

   procedure Initialise_Object_Entry
     (Item            : in out Object_Entry_Record'Class;
      Name            : in     String;
      Declaration     : access Aquarius.Trees.Root_Tree_Type'Class;
      New_Entry_Type  : access Aquarius.Types.Root_Aquarius_Type'Class;
      New_Entry_Value : in     Aquarius.Values.Aquarius_Value;
      Constant_Value  : in     Boolean);

   ------------------
   -- Frame_Offset --
   ------------------

   function Frame_Offset (Item : access Table_Entry_Record'Class)
                         return Integer
   is
   begin
      return Object_Entry (Item).Offset;
   end Frame_Offset;

   -----------------------------
   -- Initialise_Object_Entry --
   -----------------------------

   procedure Initialise_Object_Entry
     (Item            : in out Object_Entry_Record'Class;
      Name            : in     String;
      Declaration     : access Aquarius.Trees.Root_Tree_Type'Class;
      New_Entry_Type  : access Aquarius.Types.Root_Aquarius_Type'Class;
      New_Entry_Value : in     Aquarius.Values.Aquarius_Value;
      Constant_Value  : in     Boolean)
   is
   begin
      Item.Create_Entry (Name, Declaration);
      Item.Entry_Type     := Aquarius.Types.Aquarius_Type (New_Entry_Type);
      Item.Entry_Value    := New_Entry_Value;
      Item.Constant_Value := Constant_Value;
   end Initialise_Object_Entry;

   -----------------
   -- Is_Constant --
   -----------------

   function Is_Constant (Item : access Table_Entry_Record'Class)
                        return Boolean
   is
   begin
      return Object_Entry (Item).Constant_Value;
   end Is_Constant;

   ------------------
   -- Is_Intrinsic --
   ------------------

   function Is_Intrinsic (Item : access Table_Entry_Record'Class)
                         return Boolean
   is
   begin
      return Object_Entry (Item).Intrinsic;
   end Is_Intrinsic;

   ---------------------
   -- Is_Object_Entry --
   ---------------------

   function Is_Object_Entry
     (Item : access Table_Entry_Record'Class) return Boolean
   is
   begin
      return Item.all in Object_Entry_Record'Class;
   end Is_Object_Entry;

   ---------------------
   -- Is_Stack_Object --
   ---------------------

   function Is_Stack_Object (Item : access Table_Entry_Record'Class)
                            return Boolean
   is
   begin
      return Is_Object_Entry (Item);
   end Is_Stack_Object;

   ----------------------
   -- New_Object_Entry --
   ----------------------

   function New_Object_Entry
     (Name           : in     String;
      Declaration    : access Aquarius.Trees.Root_Tree_Type'Class;
      Entry_Type     : not null access
                             Aquarius.Types.Root_Aquarius_Type'Class;
      Entry_Value    : in     Aquarius.Values.Aquarius_Value;
      Constant_Value : in     Boolean)
     return Table_Entry
   is
      Result : Object_Entry_Record;
   begin
      Result.Initialise_Object_Entry (Name, Declaration,
                                      Entry_Type, Entry_Value,
                                      Constant_Value);
      return new Object_Entry_Record'(Result);
   end New_Object_Entry;

   -----------------------
   -- New_Tagatha_Entry --
   -----------------------

   function New_Tagatha_Entry
     (Name           : in     String;
      Declaration    : access Aquarius.Trees.Root_Tree_Type'Class;
      Entry_Type     : not null access
                             Aquarius.Types.Root_Aquarius_Type'Class;
      Entry_Fragment : in     Tagatha.Fragments.Tagatha_Fragment)
     return Table_Entry
   is
      Result : Object_Entry_Record;
   begin
      Result.Initialise_Object_Entry (Name, Declaration,
                                      Entry_Type, Aquarius.Values.No_Value,
                                      True);
      Result.Intrinsic := True;
      Result.Entry_Fragment := Entry_Fragment;
      return new Object_Entry_Record'(Result);
   end New_Tagatha_Entry;

   -----------------------------
   -- Object_Entry_Constraint --
   -----------------------------

   function Object_Entry_Constraint return Entry_Constraint'Class is
   begin
      return Create_Class_Constraint (Object_Entry_Record'Tag);
   end Object_Entry_Constraint;

   -----------------------------
   -- Object_Entry_Constraint --
   -----------------------------

   function Object_Entry_Constraint (Name : String)
                                    return Entry_Constraint'Class
   is
   begin
      return Create_Named_Proposition_Constraint
        (Name, Is_Object_Entry'Access);
   end Object_Entry_Constraint;

   --------------------------------
   -- Object_Entry_Default_Value --
   --------------------------------

   function Object_Entry_Default_Value
     (Item : access Table_Entry_Record'Class)
     return Aquarius.Values.Aquarius_Value
   is
   begin
      return Object_Entry (Item).Entry_Value;
   end Object_Entry_Default_Value;

   ----------------------------------
   -- Object_Entry_Intrinsic_Value --
   ----------------------------------

   function Object_Entry_Intrinsic_Value
     (Item : access Table_Entry_Record'Class)
     return Tagatha.Fragments.Tagatha_Fragment
   is
   begin
      return Object_Entry (Item).Entry_Fragment;
   end Object_Entry_Intrinsic_Value;

   -----------------------
   -- Object_Entry_Type --
   -----------------------

   function Object_Entry_Type
     (Item : access Table_Entry_Record'Class)
     return Aquarius.Types.Aquarius_Type
   is
   begin
      return Object_Entry (Item).Entry_Type;
   end Object_Entry_Type;

   ----------------------
   -- Set_Frame_Offset --
   ----------------------

   procedure Set_Frame_Offset (Item   : access Table_Entry_Record'Class;
                               Offset : in     Integer)
   is
   begin
      Object_Entry (Item).Offset := Offset;
   end Set_Frame_Offset;

end Aquarius.Entries.Objects;
