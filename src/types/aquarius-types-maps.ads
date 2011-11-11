package Aquarius.Types.Maps is

   function New_Map_Type (From : Array_Of_Types;
                          To   : Aquarius_Type)
                         return Aquarius_Type;

   function New_Map_Type (To   : Aquarius_Type)
                         return Aquarius_Type;

   function New_Map_Type return Aquarius_Type;

   procedure Add_From_Type (To        : access Root_Aquarius_Type'Class;
                            From_Type : access Root_Aquarius_Type'Class;
                            Optional  : in     Boolean);

   procedure Set_Return_Type (For_Map_Type : access Root_Aquarius_Type'Class;
                              Return_Type  : access Root_Aquarius_Type'Class);

   function Is_Map_Type (Item : Aquarius_Type) return Boolean;

   function Get_Result_Type (Item : Aquarius_Type)
                            return Aquarius_Type;

   function Get_Argument_Types (Item : Aquarius_Type)
                               return Array_Of_Types;

   function Get_Argument_Type (Item  : Aquarius_Type;
                               Index : Positive)
                              return Aquarius_Type;

   function Get_Argument_Count (Item  : access Root_Aquarius_Type'Class)
                               return Natural;

   function Get_Void_Type return Aquarius_Type;

end Aquarius.Types.Maps;
