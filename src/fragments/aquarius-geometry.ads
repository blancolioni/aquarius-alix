private with Ada.Containers.Vectors;

package Aquarius.Geometry is

   type Rectangle_GUI is interface;

   type Rectangle is interface;

   function Width (Item : Rectangle) return Positive
                   is abstract;

   function Height (Item : Rectangle) return Positive
                    is abstract;

   type Layout_Area is abstract tagged private;

   procedure Add_Item
     (Layout    : in out Layout_Area;
      Item      : not null access Rectangle'Class;
      Suggest_X : Integer;
      Suggest_Y : Integer);

   procedure On_Item_Placed
     (Layout : in out Layout_Area;
      Item   : in out Rectangle'Class;
      X, Y   : Integer)
   is abstract;

private

   type Rectangle_Access is access all Rectangle'Class;

   type Rectangle_Entry is
      record
         X, Y : Integer;
         Rec  : Rectangle_Access;
      end record;

   package Rectangle_Vectors is
     new Ada.Containers.Vectors (Positive, Rectangle_Entry);

   type Layout_Area is abstract tagged
      record
         Rectangles : Rectangle_Vectors.Vector;
      end record;

end Aquarius.Geometry;
