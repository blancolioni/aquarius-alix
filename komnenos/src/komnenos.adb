package body Komnenos is

   -----------------
   -- From_Config --
   -----------------

   function From_Config (Config : Tropos.Configuration)
                         return Layout_Rectangle
   is
   begin
      return (X => Config.Get ("x"), Y => Config.Get ("y"),
              Width => Config.Get ("width"),
              Height => Config.Get ("height"));
   end From_Config;

   ---------------
   -- To_Config --
   ---------------

   function To_Config (Rectangle : Layout_Rectangle)
                       return Tropos.Configuration
   is
      Config : Tropos.Configuration := Tropos.New_Config ("rectangle");
   begin
      Config.Add ("x", Rectangle.X);
      Config.Add ("y", Rectangle.Y);
      Config.Add ("width", Rectangle.Width);
      Config.Add ("height", Rectangle.Height);
      return Config;
   end To_Config;

end Komnenos;
