package body Aquarius.Geometry is

   function Overlaps
     (X1, Y1 : Integer;
      W1, H1 : Positive;
      X2, Y2 : Integer;
      W2, H2 : Positive)
      return Boolean;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Layout    : in out Layout_Area;
      Item      : not null access Rectangle'Class;
      Suggest_X : Integer;
      Suggest_Y : Integer)
   is
      X : Integer := Suggest_X;
      Y : constant Integer := Suggest_Y;
   begin
      for R of Layout.Rectangles loop
         if Overlaps (R.X, R.Y, R.Rec.Width, R.Rec.Height,
                      X, Y, Item.Width, Item.Height)
         then
            X := R.X + R.Rec.Width;
         end if;
      end loop;
      Layout.Rectangles.Append ((X, Y, Rectangle_Access (Item)));
      Layout_Area'Class (Layout).On_Item_Placed (Item.all, X, Y);
   end Add_Item;

   --------------
   -- Overlaps --
   --------------

   function Overlaps
     (X1, Y1 : Integer;
      W1, H1 : Positive;
      X2, Y2 : Integer;
      W2, H2 : Positive)
      return Boolean
   is
   begin
      return (X2 in X1 .. X1 + W1 and then Y2 in Y1 .. Y1 + H1)
        or else (X1 in X2 .. X2 + W2 and then Y1 in Y2 .. Y2 + H2);
   end Overlaps;

end Aquarius.Geometry;
