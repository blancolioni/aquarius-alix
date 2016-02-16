package body Komnenos.Connectors is

   Boundary_Size : constant := 16;

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
      return Connector : constant Connector_Type := new Root_Connector_Type'
        (Class              => Class,
         Source             => Entity_Visual_Access (Source),
         Source_Offset      => Source_Offset,
         Destination        => Entity_Visual_Access (Destination),
         Destination_Offset => Destination_Offset,
         Boundary           => (0, 0, 1, 1),
         Path               => (others => (0, 0)),
         Display            => null)
      do
         Connector.Update;
      end return;
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

   -------------
   -- Display --
   -------------

   function Display
     (Connector : Root_Connector_Type'Class)
      return Connector_Display
   is
   begin
      return Connector.Display;
   end Display;

   ---------------------
   -- Layout_Boundary --
   ---------------------

   function Layout_Boundary
     (Connector : Root_Connector_Type'Class)
      return Layout_Rectangle
   is
   begin
      return Connector.Boundary;
   end Layout_Boundary;

   -----------------
   -- Layout_Path --
   -----------------

   function Layout_Path
     (Connector : Root_Connector_Type'Class)
      return Layout_Line
   is
   begin
      return Connector.Path;
   end Layout_Path;

   -----------------
   -- Set_Display --
   -----------------

   procedure Set_Display
     (Connector : in out Root_Connector_Type'Class;
      Display   : not null access Connector_Display_Interface'Class)
   is
   begin
      Connector.Display := Connector_Display (Display);
   end Set_Display;

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

   ------------
   -- Update --
   ------------

   procedure Update (Connector : in out Root_Connector_Type'Class) is
      Source : Komnenos.Entities.Entity_Visual_Access renames
                 Connector.Source;
      Dest : Komnenos.Entities.Entity_Visual_Access renames
                 Connector.Destination;
      X1 : Integer renames Connector.Path (1).X;
      Y1 : Integer renames Connector.Path (1).Y;
      X2 : Integer renames Connector.Path (2).X;
      Y2 : Integer renames Connector.Path (2).Y;
      X3 : Integer renames Connector.Path (3).X;
      Y3 : Integer renames Connector.Path (3).Y;
      X4 : Integer renames Connector.Path (4).X;
      Y4 : Integer renames Connector.Path (4).Y;
   begin
      Y1 := Source.Y + Connector.Source_Offset + Boundary_Size;
      Y4 := Dest.Y + Connector.Destination_Offset + Boundary_Size;
      if Dest.X > Source.X + Source.Width then
         X1 := Source.X + Source.Width + Boundary_Size;
         X4 := Dest.X + Boundary_Size;
         X2 := (X4 + X1) / 2;
         Y2 := Y1;
         X3 := X2;
         Y3 := Y4;
      else
         X1 := Source.X + Boundary_Size;
         if Dest.X > Source.X then
            X4 := Dest.X + Boundary_Size;
            X2 := X1 - 16;
            Y2 := Y1;
            X3 := X2;
            Y3 := Y4;
         else
            X4 := Dest.X + Dest.Width + Boundary_Size;
            X2 := (X4 + X1) / 2;
            Y2 := Y1;
            X3 := X2;
            Y3 := Y4;
         end if;
      end if;

      declare
         Min_X, Min_Y : Integer := Integer'Last;
         Max_X, Max_Y : Integer := Integer'First;
      begin
         for I in Connector.Path'Range loop
            declare
               X : Integer renames Connector.Path (I).X;
               Y : Integer renames Connector.Path (I).Y;
            begin
               Min_X := Integer'Min (Min_X, X);
               Max_X := Integer'Max (Max_X, X);
               Min_Y := Integer'Min (Min_Y, Y);
               Max_Y := Integer'Max (Max_X, X);
            end;
         end loop;

         Connector.Boundary := (Min_X, Min_Y, Max_X - Min_X, Max_Y - Min_Y);
      end;

      if Connector.Display /= null then
         Connector.Display.Update;
      end if;

   end Update;

end Komnenos.Connectors;
