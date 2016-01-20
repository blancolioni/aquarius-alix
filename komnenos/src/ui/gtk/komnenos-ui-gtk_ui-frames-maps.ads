with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Strings.Fixed.Hash_Case_Insensitive;

package Komnenos.UI.Gtk_UI.Frames.Maps is
  new Ada.Containers.Indefinite_Hashed_Maps
    (Key_Type        => String,
     Element_Type    => Gtk_Frame,
     Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
     Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);
