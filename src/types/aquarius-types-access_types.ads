with Aquarius.Types.Elementary;

package Aquarius.Types.Access_Types is

   type Access_Type is
     new Aquarius.Types.Elementary.Root_Elementary_Type with private;

   overriding
   function Description
     (Item : Access_Type) return String;

   overriding
   function Create_Derived_Type
     (Item : Access_Type) return Aquarius_Type;

   function New_Access_Type (Access_To   : Aquarius_Type;
                             Access_All  : Boolean;
                             Not_Null    : Boolean)
                            return Aquarius_Type;

   function Get_Access_To (Item : Aquarius_Type) return Aquarius_Type;
   function Is_Access_All (Item : Aquarius_Type) return Boolean;
   function Is_Not_Null   (Item : Aquarius_Type) return Boolean;

   procedure Set_Access_To (Item      : Aquarius_Type;
                            Access_To : Aquarius_Type);
   procedure Set_Access_All (Item       : Aquarius_Type;
                             Access_All : Boolean);
   procedure Set_Not_Null (Item       : Aquarius_Type;
                           Not_Null : Boolean);

private

   type Access_Type is
     new Aquarius.Types.Elementary.Root_Elementary_Type with
      record
         Access_To   : Aquarius_Type;
         Access_All  : Boolean;
         Not_Null    : Boolean;
      end record;

end Aquarius.Types.Access_Types;
