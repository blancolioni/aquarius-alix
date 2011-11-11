package C64.Vicii is

   type Color is
     (Black, White, Red, Cyan, Purple, Green, Blue, Yellow,
      Orange, Brown, Light_Red, Dark_Grey, Grey, Light_Green,
      Light_Blue, Light_Grey);

   type Sprite_Index is mod 8;

   type Sprite_XY_Record is
      record
         X    : Byte;
         Y    : Byte;
      end record;

   type Sprite_XY_Array is
     array (Sprite_Index) of Sprite_XY_Record;

   type Sprite_Flags is
     array (Sprite_Index) of Boolean;

   Sprite_XY     : Sprite_XY_Array;
   Sprite_X_MSB  : Sprite_Flag;
   Sprite_Enable : Sprite_Flag;

   type Memory_Pointer_Byte is
      record
         Vic_Memory     : Word_4;
         Character_Bank : Word_3;
         Unused         : Bit;
      end record;

   Border_Color     : Color;
   Background_Color : Color;

end C64.Vicii;
