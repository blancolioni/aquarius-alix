private with Ada.Containers.Doubly_Linked_Lists;

with Komnenos.Fragments;
with Komnenos.Session_Objects;

package Komnenos.Layouts is

   Margin : constant := 32;

   type Root_Layout_Type is
     abstract new Komnenos.Session_Objects.Session_Object_Interface
   with private;

   procedure Item_Moved
     (Layout : in out Root_Layout_Type;
      Item   : Komnenos.Fragments.Fragment_Type)
   is abstract;

   procedure Item_Placed
     (Layout : in out Root_Layout_Type;
      Item   : Komnenos.Fragments.Fragment_Type)
   is abstract;

   procedure Place_Item
     (Layout    : in out Root_Layout_Type'Class;
      Fragment  : Komnenos.Fragments.Fragment_Type;
      Reference : Komnenos.Fragments.Fragment_Type;
      Offset    : Integer);
   --  Place Fragment in the Layout such that it sits to the right
   --  of Reference with vertical Offset.  Move other fragments
   --  around as necessary

   procedure Place_Item
     (Layout    : in out Root_Layout_Type'Class;
      Fragment  : Komnenos.Fragments.Fragment_Type);
   --  Place Fragment in Layout towards the top left of the current view port
   --  Try not to move anything else around

   procedure Move_Item
     (Layout    : in out Root_Layout_Type'Class;
      Fragment  : Komnenos.Fragments.Fragment_Type);
   --  Update the position of Fragment.  Move other fragments around to
   --  accommodate it

   type Layout_Type is access all Root_Layout_Type'Class;

private

   package Fragment_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Komnenos.Fragments.Fragment_Type,
        Komnenos.Fragments."=");

   type Root_Layout_Type is
   abstract new Komnenos.Session_Objects.Session_Object_Interface with
      record
         Items : Fragment_Lists.List;
      end record;

end Komnenos.Layouts;
