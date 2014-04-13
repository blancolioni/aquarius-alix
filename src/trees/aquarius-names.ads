private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

package Aquarius.Names is

   pragma Preelaborate;

   --  A package for handling Strings biased for short strings
   --  that are initialised once and never changed afterwards

   type Aquarius_Name is private;

   function To_Aquarius_Name (Item : String) return Aquarius_Name;
   function To_String (Item : Aquarius_Name) return String;

   function "=" (Left  : Aquarius_Name;
                 Right : String)
                return Boolean;
   function "=" (Left  : String;
                 Right : Aquarius_Name)
                return Boolean;
   overriding
   function "=" (Left, Right : Aquarius_Name) return Boolean;

   function Null_Aquarius_Name return Aquarius_Name;

   type Name_List is private;

   procedure Clear (Item : in out Name_List);
   function Empty return Name_List;

   procedure Append (To   : in out Name_List;
                     Name : in     String);
   function Count (Item : Name_List) return Natural;
   function Element (Item  : Name_List;
                     Index : Positive)
                    return String;
   function Contains (Item : Name_List;
                      Name : String)
                     return Boolean;

   type Aquarius_Name_Value is new Root_Aquarius_Object with private;

   overriding
   function Name (Item : Aquarius_Name_Value)
                 return String;

   function Name_Value (Name : String)
                       return access Aquarius_Name_Value'Class;

private

   type Aquarius_Name is new Ada.Strings.Unbounded.Unbounded_String;

   package Name_Vectors is
      new Ada.Containers.Vectors (Positive, Aquarius_Name);

   type Name_List is
      record
         Names : Name_Vectors.Vector;
      end record;

   type Aquarius_Name_Value is new Root_Aquarius_Object with
      record
         Value : Aquarius_Name;
      end record;

end Aquarius.Names;
