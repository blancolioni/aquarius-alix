with Aquarius.Values;

package Aquarius.Types.Subtypes is

   function New_Subtype
     (From_Type   : access Root_Aquarius_Type'Class)
     return Aquarius_Type;

   function New_Subtype
     (From_Type   : access Root_Aquarius_Type'Class;
      Low, High   : in     Aquarius.Values.Aquarius_Value)
     return Aquarius_Type;

end Aquarius.Types.Subtypes;
