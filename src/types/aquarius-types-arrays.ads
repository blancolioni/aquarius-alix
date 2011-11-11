package Aquarius.Types.Arrays is

   function New_Array_Type
     (Index_Type     : access Root_Aquarius_Type'Class;
      Component_Type : access Root_Aquarius_Type'Class;
      Constrained    : in Boolean)
     return Aquarius_Type;

   function New_Array_Type return Aquarius_Type;

   function Is_Array_Type (Item : Aquarius_Type) return Boolean;
   function Get_Index_Type (Item : Aquarius_Type) return Aquarius_Type;
   function Get_Component_Type (Item : Aquarius_Type) return Aquarius_Type;

   procedure Set_Index_Type (For_Array_Type : access Root_Aquarius_Type'Class;
                             Index_Type     : access Root_Aquarius_Type'Class;
                             Constrained    : in     Boolean);

   procedure Set_Component_Type (For_Array_Type : Aquarius_Type;
                                 Component_Type : Aquarius_Type);

end Aquarius.Types.Arrays;
