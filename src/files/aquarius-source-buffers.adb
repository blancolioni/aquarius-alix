with Aquarius.Buffers;

package body Aquarius.Source.Buffers is

   type Buffer_File_Type is limited new Source_File_Record with
      record
         Buffer : Aquarius.Buffers.Aquarius_Buffer;
      end record;

   type Buffer_File_Access is access all Buffer_File_Type'Class;

   overriding
   function Get_File_Name (File : access Buffer_File_Type) return String;

   overriding
   function Get_Full_Path (File : access Buffer_File_Type) return String;

   overriding
   procedure Next_Line (File : access Buffer_File_Type);

   overriding
   function End_Of_File (File : access Buffer_File_Type) return Boolean;

   overriding
   procedure Close (Item : access Buffer_File_Type);

   -----------------
   -- Buffer_File --
   -----------------

   function Buffer_File
     (Buffer : access Aquarius.Buffers.Aquarius_Buffer_Record'Class)
     return Source_File
   is
      Result : constant Buffer_File_Access := new Buffer_File_Type;
   begin
      Result.Buffer := Aquarius.Buffers.Aquarius_Buffer (Buffer);
      return Source_File (Result);
   end Buffer_File;

   -----------
   -- Close --
   -----------

   overriding
   procedure Close (Item : access Buffer_File_Type) is
      pragma Unreferenced (Item);
   begin
      null;
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   overriding
   function End_Of_File (File : access Buffer_File_Type) return Boolean is
      pragma Unreferenced (File);
   begin
      return False;
   end End_Of_File;

   -------------------
   -- Get_File_Name --
   -------------------

   overriding
   function Get_File_Name (File : access Buffer_File_Type) return String is
   begin
      return File.Buffer.Name;
   end Get_File_Name;

   -------------------
   -- Get_Full_Path --
   -------------------

   overriding
   function Get_Full_Path (File : access Buffer_File_Type) return String is
   begin
      return File.Buffer.Full_Path;
   end Get_Full_Path;

   ---------------
   -- Next_Line --
   ---------------

   overriding
   procedure Next_Line (File : access Buffer_File_Type) is
      pragma Unreferenced (File);
   begin
      null;
   end Next_Line;

end Aquarius.Source.Buffers;
