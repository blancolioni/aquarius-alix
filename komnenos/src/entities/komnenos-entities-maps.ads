with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Strings.Fixed.Hash_Case_Insensitive;

package Komnenos.Entities.Maps is
  new Ada.Containers.Indefinite_Hashed_Maps
    (Key_Type        => String,
     Element_Type    => Entity_Reference,
     Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
     Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);
