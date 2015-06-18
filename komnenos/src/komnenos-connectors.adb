package body Komnenos.Connectors is

   -----------
   -- Class --
   -----------

   function Class
     (Connector : Root_Connector_Type'Class)
      return Connector_Class
   is
   begin
      return Connector.Class;
   end Class;

   -------------
   -- Connect --
   -------------

   function Connect
     (Class              : Connector_Class;
      Source             : not null access
        Komnenos.Entities.Entity_Visual'Class;
      Source_Offset      : Integer;
      Destination        : not null access
        Komnenos.Entities.Entity_Visual'Class;
      Destination_Offset : Integer)
      return Connector_Type
   is
      use Komnenos.Entities;
   begin
      return new Root_Connector_Type'
        (Class, Entity_Visual_Access (Source), Source_Offset,
         Entity_Visual_Access (Destination), Destination_Offset);
   end Connect;

   -----------------
   -- Destination --
   -----------------

   function Destination
     (Connector : Root_Connector_Type'Class)
      return Komnenos.Entities.Entity_Visual_Access
   is
   begin
      return Connector.Destination;
   end Destination;

   ------------------------
   -- Destination_Offset --
   ------------------------

   function Destination_Offset
     (Connector : Root_Connector_Type'Class)
      return Integer
   is
   begin
      return Connector.Destination_Offset;
   end Destination_Offset;

   ------------
   -- Source --
   ------------

   function Source
     (Connector : Root_Connector_Type'Class)
      return Komnenos.Entities.Entity_Visual_Access
   is
   begin
      return Connector.Source;
   end Source;

   -------------------
   -- Source_Offset --
   -------------------

   function Source_Offset
     (Connector : Root_Connector_Type'Class)
      return Integer
   is
   begin
      return Connector.Source_Offset;
   end Source_Offset;

end Komnenos.Connectors;
