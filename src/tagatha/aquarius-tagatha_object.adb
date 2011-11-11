package body Aquarius.Tagatha_Object is

   ---------------------------
   -- Create_Tagatha_Object --
   ---------------------------

   function Create_Tagatha_Object
     (From_Unit : Tagatha.Units.Tagatha_Unit_Access)
     return Aquarius_Tagatha_Object
   is
   begin
      return new Aquarius_Tagatha_Record'(Unit => From_Unit);
   end Create_Tagatha_Object;

   --------------
   -- Generate --
   --------------

   procedure Generate (Item : access Aquarius_Tagatha_Record'Class;
                       Arch : in     String)
   is
   begin
      Item.Unit.Optimise;
      Item.Unit.Write (Arch);
   end Generate;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit (From : access Aquarius_Tagatha_Record'Class)
                     return Tagatha.Units.Tagatha_Unit_Access
   is
   begin
      return From.Unit;
   end Get_Unit;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Item : Aquarius_Tagatha_Record)
                 return String
   is
   begin
      return Item.Unit.External_Name;
   end Name;

   ----------
   -- Push --
   ----------

   procedure Push (Unit    : access Aquarius_Tagatha_Record'Class;
                   Value   : in     Tagatha.Tagatha_Integer;
                   Size    : in     Tagatha.Tagatha_Size :=
                     Tagatha.Default_Integer_Size)
   is
   begin
      Unit.Unit.Push (Value, Size);
   end Push;

end Aquarius.Tagatha_Object;
