package Aquarius.Types.Tagged_Types is

   function New_Interface_Type return Aquarius_Type;
   function New_Tagged_Record (Is_Abstract : Boolean;
                               Parent      : Aquarius_Type)
                              return Aquarius_Type;

   function New_Tagged_Record (Parent : Aquarius_Type)
                              return Aquarius_Type;

   function Is_Tagged (Item : access Root_Aquarius_Type'Class)
                      return Boolean;

end Aquarius.Types.Tagged_Types;
