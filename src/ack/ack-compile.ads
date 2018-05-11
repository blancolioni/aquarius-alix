with Aqua.Images;

with Ack.Classes;

package Ack.Compile is

   procedure Compile_Class
     (Source_Path : String;
      To_Image    : Aqua.Images.Image_Type;
      Feature_Callback : access
        procedure (Class        : not null access constant
                     Ack.Classes.Class_Entity_Record'Class;
                   Feature      : not null access constant
                     Root_Entity_Type'Class));

   procedure Load_Root_Class
     (Source_Path : String;
      To_Image    : Aqua.Images.Image_Type);

   procedure Load_Link_Config
     (Image : Aqua.Images.Image_Type);

end Ack.Compile;
