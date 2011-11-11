package Aquarius.Types.Private_Types is

   function New_Private_Type return Aquarius_Type;

   function Is_Private_Type
     (Item : access Root_Aquarius_Type'Class)
     return Boolean;

   procedure Set_Full_Type
     (Item      : access Root_Aquarius_Type'Class;
      Full_Type : access Root_Aquarius_Type'Class);

end Aquarius.Types.Private_Types;
