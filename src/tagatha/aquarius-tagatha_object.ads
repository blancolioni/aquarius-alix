with Tagatha.Units;

package Aquarius.Tagatha_Object is

   type Aquarius_Tagatha_Record is
     new Root_Aquarius_Object with private;

   type Aquarius_Tagatha_Object is
     access all Aquarius_Tagatha_Record'Class;

   function Create_Tagatha_Object
     (From_Unit : Tagatha.Units.Tagatha_Unit_Access)
     return Aquarius_Tagatha_Object;

   function Get_Unit (From : access Aquarius_Tagatha_Record'Class)
                     return Tagatha.Units.Tagatha_Unit_Access;

   procedure Generate (Item : access Aquarius_Tagatha_Record'Class;
                       Arch : in     String);

   procedure Push (Unit    : access Aquarius_Tagatha_Record'Class;
                   Value   : in     Tagatha.Tagatha_Integer;
                   Size    : in     Tagatha.Tagatha_Size :=
                     Tagatha.Default_Integer_Size);

private

   type Aquarius_Tagatha_Record is
     new Root_Aquarius_Object with
      record
         Unit : Tagatha.Units.Tagatha_Unit_Access;
      end record;

   overriding
   function Name (Item : Aquarius_Tagatha_Record)
                 return String;

end Aquarius.Tagatha_Object;
