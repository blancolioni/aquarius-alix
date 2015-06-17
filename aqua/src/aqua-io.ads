private with Ada.Sequential_IO;

package Aqua.IO is

   type File_Type is limited private;

   procedure Create (File : in out File_Type;
                     Path : String);

   procedure Open (File : in out File_Type;
                   Path : String);

   procedure Close (File : in out File_Type);

   procedure Write_Byte
     (File  : File_Type;
      Value : Byte);

   procedure Read_Byte
     (File  : File_Type;
      Value : out Byte);

   procedure Write_Word
     (File  : File_Type;
      Value : Word);

   procedure Read_Word
     (File  : File_Type;
      Value : out Word);

   procedure Write_Address
     (File  : File_Type;
      Value : Address);

   procedure Read_Address
     (File  : File_Type;
      Value : out Address);

   procedure Write_String_Literal
     (File  : File_Type;
      Value : String);

   function Read_String_Literal
     (File : File_Type)
      return String;

   function Hex_Image
     (Value : Word)
      return String;

   function Octal_Image
     (Value : Word)
      return String;

   function Hex_Image
     (Value : Address)
      return String;

   function Hex_Image
     (Value : Byte)
      return String;

private

   package Byte_IO is
     new Ada.Sequential_IO (Byte);

   type File_Type is limited
      record
         F : Byte_IO.File_Type;
      end record;

end Aqua.IO;
