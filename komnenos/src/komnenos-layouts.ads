private with Ada.Containers.Doubly_Linked_Lists;

with Tropos;

with Komnenos.Connectors;
with Komnenos.Fragments;
with Komnenos.Session_Objects;

package Komnenos.Layouts is

   Margin : constant := 64;

   type Root_Layout_Type is
     abstract new Komnenos.Session_Objects.Session_Object_Interface
   with private;

   overriding function Config_Name (Layout : Root_Layout_Type) return String
   is ("layout");

   overriding procedure To_Config
     (Layout : Root_Layout_Type;
      Config : in out Tropos.Configuration);

   overriding procedure From_Config
     (Layout : not null access Root_Layout_Type;
      Config : Tropos.Configuration);

   procedure Item_Moved
     (Layout : in out Root_Layout_Type;
      Item   : Komnenos.Fragments.Fragment_Type);

   procedure Item_Placed
     (Layout : in out Root_Layout_Type;
      Item   : Komnenos.Fragments.Fragment_Type)
   is abstract;

   procedure Item_Removed
     (Layout : in out Root_Layout_Type;
      Item   : Komnenos.Fragments.Fragment_Type);

   procedure Connection
     (Layout : in out Root_Layout_Type;
      Connector : Komnenos.Connectors.Connector_Type);

   procedure Update_Connector
     (Layout    : in out Root_Layout_Type;
      Connector : Komnenos.Connectors.Connector_Type)
   is abstract;

   procedure Hide_Connector
     (Layout    : in out Root_Layout_Type;
      Connector : Komnenos.Connectors.Connector_Type)
   is abstract;

   procedure Set_Full_Size
     (Layout      : in out Root_Layout_Type;
      Full_Width  : Natural;
      Full_Height : Natural);

   function Full_Width (Layout : Root_Layout_Type) return Natural;
   function Full_Height (Layout : Root_Layout_Type) return Natural;

   procedure Set_Visible_Area
     (Layout        : in out Root_Layout_Type;
      Left, Top     : Integer;
      Width, Height : Positive);

   function Visible_Left
     (Layout : Root_Layout_Type'Class)
      return Integer;

   function Visible_Top
     (Layout : Root_Layout_Type'Class)
      return Integer;

   function Visible_Width
     (Layout : Root_Layout_Type'Class)
      return Positive;

   function Visible_Height
     (Layout : Root_Layout_Type'Class)
      return Positive;

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

   procedure Remove_Item
     (Layout    : in out Root_Layout_Type'Class;
      Fragment  : Komnenos.Fragments.Fragment_Type);

   procedure Scan
     (Layout  : Root_Layout_Type'Class;
      Process : not null access
        procedure (Fragment : Komnenos.Fragments.Fragment_Type));

   procedure Scan_Connections
     (Layout  : Root_Layout_Type'Class;
      Process : not null access
        procedure (Connection : Komnenos.Connectors.Connector_Type));

   function Find_Fragment
     (Layout  : Root_Layout_Type'Class;
      Key     : String)
      return Komnenos.Fragments.Fragment_Type;

   type Layout_Type is access all Root_Layout_Type'Class;

private

   package Fragment_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Komnenos.Fragments.Fragment_Type,
        Komnenos.Fragments."=");

   package Connector_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Komnenos.Connectors.Connector_Type,
        Komnenos.Connectors."=");

   type Root_Layout_Type is
   abstract new Komnenos.Session_Objects.Session_Object_Interface with
      record
         Items          : Fragment_Lists.List;
         Connectors     : Connector_Lists.List;
         Full_Width     : Natural := 1;
         Full_Height    : Natural := 1;
         Visible_Left   : Integer := 0;
         Visible_Top    : Integer := 0;
         Visible_Width  : Positive := 1;
         Visible_Height : Positive := 1;
      end record;

end Komnenos.Layouts;
