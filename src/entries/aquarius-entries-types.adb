with Ada.Tags;

package body Aquarius.Entries.Types is

   type Type_Entry_Record is new Table_Entry_Record with
      record
         Entry_Type : Aquarius.Types.Aquarius_Type;
      end record;

   -------------------
   -- Complete_Type --
   -------------------

   procedure Complete_Type (Item      : in Table_Entry;
                            Full_Type : in Aquarius.Types.Aquarius_Type)
   is
   begin
      Type_Entry_Record (Item.all).Entry_Type := Full_Type;
   end Complete_Type;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Item : Table_Entry)
     return Aquarius.Types.Aquarius_Type
   is
   begin
      if Item.all not in Type_Entry_Record'Class then
         raise Constraint_Error with
           "attempt to get type from an entry of type " &
           Ada.Tags.External_Tag (Item.all'Tag);
      end if;
      return Type_Entry_Record (Item.all).Entry_Type;
   end Get_Type;

   ----------------------
   -- Is_Complete_Type --
   ----------------------

   function Is_Complete_Type (Item : Table_Entry) return Boolean is
      use type Aquarius.Types.Aquarius_Type;
   begin
      return Type_Entry_Record (Item.all).Entry_Type /= null;
   end Is_Complete_Type;

   -------------------
   -- Is_Type_Entry --
   -------------------

   function Is_Type_Entry
     (Item : Table_Entry)
     return Boolean
   is
   begin
      return Item.all in Type_Entry_Record'Class;
   end Is_Type_Entry;

   --------------------
   -- New_Type_Entry --
   --------------------

   function New_Type_Entry
     (Name        : in     String;
      Declaration : access Aquarius.Trees.Root_Tree_Type'Class;
      Entry_Type  : access Aquarius.Types.Root_Aquarius_Type'Class)
     return Table_Entry
   is
      Result : Type_Entry_Record;
   begin
      Result.Create_Entry (Name, Declaration);
      Result.Entry_Type := Aquarius.Types.Aquarius_Type (Entry_Type);
      return new Type_Entry_Record'(Result);
   end New_Type_Entry;

   --------------------
   -- New_Type_Entry --
   --------------------

   function New_Type_Entry
     (Name        : in     String;
      Declaration : access Aquarius.Trees.Root_Tree_Type'Class)
     return Table_Entry
   is
      Result : Type_Entry_Record;
   begin
      Result.Create_Entry (Name, Declaration);
      Result.Entry_Type := null;
      return new Type_Entry_Record'(Result);
   end New_Type_Entry;

end Aquarius.Entries.Types;
