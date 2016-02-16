with Komnenos.Entities;

package Komnenos.Connectors is

   type Connector_Class is (Arrow, Dashed_Arrow);

   type Root_Connector_Type is tagged private;

   function Class
     (Connector : Root_Connector_Type'Class)
      return Connector_Class;

   function Source
     (Connector : Root_Connector_Type'Class)
      return Komnenos.Entities.Entity_Visual_Access;

   function Source_Offset
     (Connector : Root_Connector_Type'Class)
      return Integer;

   function Destination
     (Connector : Root_Connector_Type'Class)
      return Komnenos.Entities.Entity_Visual_Access;

   function Destination_Offset
     (Connector : Root_Connector_Type'Class)
      return Integer;

   function Layout_Boundary
     (Connector : Root_Connector_Type'Class)
      return Layout_Rectangle;

   function Layout_Path
     (Connector : Root_Connector_Type'Class)
      return Layout_Line;

   procedure Update (Connector : in out Root_Connector_Type'Class);

   type Connector_Type is access all Root_Connector_Type'Class;

   function Connect
     (Class              : Connector_Class;
      Source             : not null access
        Komnenos.Entities.Entity_Visual'Class;
      Source_Offset      : Integer;
      Destination        : not null access
        Komnenos.Entities.Entity_Visual'Class;
      Destination_Offset : Integer)
      return Connector_Type;

   type Connector_Display_Interface is interface;

   procedure Remove (Display : in out Connector_Display_Interface)
   is abstract;

   procedure Update (Display : in out Connector_Display_Interface)
   is abstract;

   type Connector_Display is access all Connector_Display_Interface'Class;

   procedure Set_Display
     (Connector : in out Root_Connector_Type'Class;
      Display   : not null access Connector_Display_Interface'Class);

   function Display
     (Connector : Root_Connector_Type'Class)
      return Connector_Display;

private

   type Root_Connector_Type is tagged
      record
         Class              : Connector_Class;
         Source             : Komnenos.Entities.Entity_Visual_Access;
         Source_Offset      : Integer;
         Destination        : Komnenos.Entities.Entity_Visual_Access;
         Destination_Offset : Integer;
         Boundary           : Layout_Rectangle;
         Path               : Layout_Line (1 .. 4);
         Display            : Connector_Display;
      end record;

end Komnenos.Connectors;
