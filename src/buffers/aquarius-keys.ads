package Aquarius.Keys is

   type Aquarius_Key is private;

   function Shift   (Key : Aquarius_Key) return Boolean;
   function Control (Key : Aquarius_Key) return Boolean;
   function Mod1    (Key : Aquarius_Key) return Boolean;
   function Mod2    (Key : Aquarius_Key) return Boolean;

   function Modify (Key     : Aquarius_Key;
                    Shift   : Boolean       := False;
                    Control : Boolean       := False;
                    Mod1    : Boolean       := False;
                    Mod2    : Boolean       := False)
                   return Aquarius_Key;

   function Is_Character (Key  : Aquarius_Key) return Boolean;
   --  Return True if Key represents a self-inserting character

   function Is_Function  (Key : Aquarius_Key) return Boolean;
   --  return True if Key represents a function key

   function Character_Key (Ch    : Character) return Aquarius_Key;
   function Get_Character (Key : Aquarius_Key) return Character;

   function Raw_Key (Key : Aquarius_Key) return Aquarius_Key;

   Null_Key    : constant Aquarius_Key;
   Tab         : constant Aquarius_Key;
   Left_Tab    : constant Aquarius_Key;
   Up_Arrow    : constant Aquarius_Key;
   Down_Arrow  : constant Aquarius_Key;
   Left_Arrow  : constant Aquarius_Key;
   Right_Arrow : constant Aquarius_Key;
   Page_Up     : constant Aquarius_Key;
   Page_Down   : constant Aquarius_Key;
   Home_Key    : constant Aquarius_Key;
   End_Key     : constant Aquarius_Key;
   Back_Space  : constant Aquarius_Key;
   Line_Feed   : constant Aquarius_Key;

private

   type Aquarius_Key is mod 16#10_0000#;
   subtype Real_Key is Aquarius_Key range 1 .. 16#FFFF#;

   Null_Key    : constant Aquarius_Key := 16#0000#;

   Tab         : constant Aquarius_Key := 16#F000#;
   Left_Tab    : constant Aquarius_Key := 16#F001#;
   Up_Arrow    : constant Aquarius_Key := 16#F002#;
   Down_Arrow  : constant Aquarius_Key := 16#F003#;
   Left_Arrow  : constant Aquarius_Key := 16#F004#;
   Right_Arrow : constant Aquarius_Key := 16#F005#;
   Page_Up     : constant Aquarius_Key := 16#F006#;
   Page_Down   : constant Aquarius_Key := 16#F007#;
   Home_Key    : constant Aquarius_Key := 16#F008#;
   End_Key     : constant Aquarius_Key := 16#F009#;
   Back_Space  : constant Aquarius_Key := 16#F00A#;
   Line_Feed   : constant Aquarius_Key := 16#F00B#;

   subtype Function_Keys is Aquarius_Key range 16#F100# .. 16#F1FF#;

   Key_Mask     : constant Aquarius_Key := 16#FFFF#;
   Shift_Mask   : constant Aquarius_Key := 16#1_0000#;
   Control_Mask : constant Aquarius_Key := 16#2_0000#;
   Mod1_Mask    : constant Aquarius_Key := 16#4_0000#;
   Mod2_Mask    : constant Aquarius_Key := 16#8_0000#;

end Aquarius.Keys;
