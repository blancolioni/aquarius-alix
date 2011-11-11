with Aquarius.Entries;
with Aquarius.Types.Composite;
with Aquarius.Types.Records;

package body Aquarius.Types.Tagged_Types is

   type Tagged_Type is new Aquarius.Types.Composite.Root_Composite_Type with
      record
         Parent           : Aquarius_Type;
         Interfaces       : Type_Vector.Vector;
         Components       : Aquarius_Type;
         Primitives       : Aquarius.Entries.Symbol_Table;
         Is_Abstract      : Boolean;
         Is_Interface     : Boolean;
         Is_Synchronized  : Boolean;
      end record;

   overriding
   function Create_Derived_Type
     (Item : Tagged_Type) return Aquarius_Type;

   overriding
   function Description (Item : Tagged_Type) return String;

   procedure Initialise_Tagged_Type
     (Item            : in out Tagged_Type'Class;
      Is_Abstract     : in     Boolean    := False;
      Is_Interface    : in     Boolean    := False;
      Is_Synchronized : in     Boolean    := False);

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   overriding
   function Create_Derived_Type
     (Item : Tagged_Type)
     return Aquarius_Type
   is
   begin
      return new Tagged_Type'(Item);
   end Create_Derived_Type;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description (Item : Tagged_Type) return String is
      pragma Unreferenced (Item);
   begin
      return "a tagged type";
   end Description;

   ----------------------------
   -- Initialise_Tagged_Type --
   ----------------------------

   procedure Initialise_Tagged_Type
     (Item            : in out Tagged_Type'Class;
      Is_Abstract     : in     Boolean    := False;
      Is_Interface    : in     Boolean    := False;
      Is_Synchronized : in     Boolean    := False)
   is
   begin
      Item.Is_Abstract     := Is_Abstract;
      Item.Is_Interface    := Is_Interface;
      Item.Is_Synchronized := Is_Synchronized;
      Item.Primitives      :=
        Aquarius.Entries.New_Symbol_Table ("tagged type primitives");
      if not Is_Interface then
         Item.Components := Aquarius.Types.Records.New_Record_Type;
      end if;
   end Initialise_Tagged_Type;

   ---------------
   -- Is_Tagged --
   ---------------

   function Is_Tagged (Item : access Root_Aquarius_Type'Class)
                      return Boolean
   is
   begin
      return Item.all in Tagged_Type'Class;
   end Is_Tagged;

   ------------------------
   -- New_Interface_Type --
   ------------------------

   function New_Interface_Type return Aquarius_Type is
      New_Type : Tagged_Type;
   begin
      Initialise_Tagged_Type (New_Type, Is_Abstract => False,
                              Is_Interface => True);
      return new Tagged_Type'(New_Type);
   end New_Interface_Type;

   -----------------------
   -- New_Tagged_Record --
   -----------------------

   function New_Tagged_Record (Is_Abstract : Boolean;
                               Parent      : Aquarius_Type)
                              return Aquarius_Type
   is
      Result : Tagged_Type;
   begin
      Initialise_Tagged_Type (Result, Is_Abstract => Is_Abstract);
      Result.Parent := Parent;
      return new Tagged_Type'(Result);
   end New_Tagged_Record;

   -----------------------
   -- New_Tagged_Record --
   -----------------------

   function New_Tagged_Record (Parent : Aquarius_Type)
                              return Aquarius_Type
   is
   begin
      return New_Tagged_Record (False, Parent);
   end New_Tagged_Record;

end Aquarius.Types.Tagged_Types;
