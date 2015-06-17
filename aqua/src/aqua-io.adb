package body Aqua.IO is

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      Byte_IO.Close (File.F);
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in out File_Type;
      Path : String)
   is
   begin
      Byte_IO.Create (File.F, Byte_IO.Out_File, Path);
   end Create;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image
     (Value : Word)
      return String
   is
      Hex_Digits : constant String := "0123456789ABCDEF";
      Result     : String (1 .. 4);
      Acc        : Natural := Natural (Value);
   begin
      for I in reverse Result'Range loop
         Result (I) := Hex_Digits (Acc mod 16 + 1);
         Acc := Acc / 16;
      end loop;
      return Result;
   end Hex_Image;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image
     (Value : Address)
      return String
   is
   begin
      return Hex_Image (Word (Value));
   end Hex_Image;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image
     (Value : Byte)
      return String
   is
      Result : constant String := Hex_Image (Word (Value) * 256);
   begin
      return Result (1 .. 2);
   end Hex_Image;

   -----------------
   -- Octal_Image --
   -----------------

   function Octal_Image
     (Value : Word)
      return String
   is
      Octal_Digits : constant String := "01234567";
      Result     : String (1 .. 6);
      Acc        : Natural := Natural (Value);
   begin
      for I in reverse Result'Range loop
         Result (I) := Octal_Digits (Acc mod 8 + 1);
         Acc := Acc / 8;
      end loop;
      return Result;
   end Octal_Image;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out File_Type;
      Path : String)
   is
   begin
      Byte_IO.Open (File.F, Byte_IO.In_File, Path);
   end Open;

   ------------------
   -- Read_Address --
   ------------------

   procedure Read_Address
     (File  : File_Type;
      Value : out Address)
   is
      W : Word;
   begin
      Read_Word (File, W);
      Value := Get_Address (W);
   end Read_Address;

   ---------------
   -- Read_Byte --
   ---------------

   procedure Read_Byte
     (File  : File_Type;
      Value : out Byte)
   is
   begin
      Byte_IO.Read (File.F, Value);
   end Read_Byte;

   -------------------------
   -- Read_String_Literal --
   -------------------------

   function Read_String_Literal
     (File : File_Type)
      return String
   is
      Length : Word;
   begin
      Read_Word (File, Length);
      return Result : String (1 .. Natural (Length)) do
         for I in Result'Range loop
            declare
               X : Byte;
            begin
               Read_Byte (File, X);
               Result (I) := Character'Val (X);
            end;
         end loop;
      end return;
   end Read_String_Literal;

   ---------------
   -- Read_Word --
   ---------------

   procedure Read_Word
     (File  : File_Type;
      Value : out Word)
   is
      X : Byte;
   begin
      Read_Byte (File, X);
      Value := Word (X);
      Read_Byte (File, X);
      Value := Value + Word (X) * 256;
   end Read_Word;

   -------------------
   -- Write_Address --
   -------------------

   procedure Write_Address
     (File  : File_Type;
      Value : Address)
   is
   begin
      Write_Word (File, To_Address_Word (Value));
   end Write_Address;

   ----------------
   -- Write_Byte --
   ----------------

   procedure Write_Byte
     (File  : File_Type;
      Value : Byte)
   is
   begin
      Byte_IO.Write (File.F, Value);
   end Write_Byte;

   --------------------------
   -- Write_String_Literal --
   --------------------------

   procedure Write_String_Literal
     (File  : File_Type;
      Value : String)
   is
   begin
      Write_Word (File, Word (Value'Length));
      for Ch of Value loop
         Write_Byte (File, Character'Pos (Ch));
      end loop;
   end Write_String_Literal;

   ----------------
   -- Write_Word --
   ----------------

   procedure Write_Word
     (File  : File_Type;
      Value : Word)
   is
   begin
      Write_Byte (File, Byte (Value mod 256));
      Write_Byte (File, Byte (Value / 256));
   end Write_Word;

end Aqua.IO;
