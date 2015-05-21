private with Ada.Containers.Indefinite_Hashed_Sets;
private with Ada.Strings.Fixed.Hash_Case_Insensitive;
private with Ada.Strings.Fixed.Equal_Case_Insensitive;

package Aquarius.Properties.String_Sets is

   type String_Set_Property_Type is
     new Root_Aquarius_Object with private;

   overriding function Name
     (Item : String_Set_Property_Type)
      return String;

   procedure Include
     (Property : in out String_Set_Property_Type'Class;
      Value    : in     String);

   function Member
     (Property : String_Set_Property_Type'Class;
      Value    : String)
      return Boolean;

   procedure Iterate
     (Property : String_Set_Property_Type'Class;
      Process  : not null access
        procedure (Value : String));

private

   package String_Sets is
     new Ada.Containers.Indefinite_Hashed_Sets
       (Element_Type        => String,
        Hash                => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Elements => Ada.Strings.Fixed.Equal_Case_Insensitive);

   type String_Set_Property_Type is
     new Root_Aquarius_Object with
      record
         Set : String_Sets.Set;
      end record;

end Aquarius.Properties.String_Sets;
