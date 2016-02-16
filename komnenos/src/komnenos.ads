with Tropos;

package Komnenos is

   type Layout_Rectangle is
      record
         X, Y          : Integer;
         Width, Height : Positive;
      end record;

   function To_Config (Rectangle : Layout_Rectangle)
                       return Tropos.Configuration;

   function From_Config (Config : Tropos.Configuration)
                         return Layout_Rectangle;

   type Layout_Point is
      record
         X, Y : Integer;
      end record;

   type Layout_Line is array (Positive range <>) of Layout_Point;

end Komnenos;
