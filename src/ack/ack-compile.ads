with Aqua.Images;

with Ack.Classes;

package Ack.Compile is

   procedure Compile_Class
     (Source_Path : String;
      To_Image    : Aqua.Images.Image_Type;
      Feature_Callback : access
        procedure (Class        : not null access constant
                     Ack.Classes.Class_Entity_Record'Class;
                   Feature_Name : String;
                   Child_Name   : String;
                   Child_Type   : Entity_Type));

end Ack.Compile;
