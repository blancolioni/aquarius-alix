with Aqua.Images;

package Aquarius.Ack.Compile is

   procedure Compile_Class
     (Source_Path : String;
      To_Image    : Aqua.Images.Image_Type;
      Feature_Callback : access
        procedure (Class        : Entity_Id;
                   Feature_Name : String;
                   Child_Name   : String;
                   Child_Type   : Entity_Id));

end Aquarius.Ack.Compile;
