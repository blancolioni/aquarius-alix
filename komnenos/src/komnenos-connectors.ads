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

private

   type Root_Connector_Type is tagged
      record
         Class              : Connector_Class;
         Source             : Komnenos.Entities.Entity_Visual_Access;
         Source_Offset      : Integer;
         Destination        : Komnenos.Entities.Entity_Visual_Access;
         Destination_Offset : Integer;
      end record;

end Komnenos.Connectors;
