private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

with As.Files;

private package As.Objects is

   type Instance is tagged private;
   type Reference is access all Instance'Class;

   function Create return Reference;

   procedure Set_Location
     (This  : in out Instance'Class;
      Value : Word_32);

   function Location
     (This : Instance'Class)
      return Word_32;

   procedure Set_Context
     (This    : in out Instance'Class;
      Context : As.Files.File_Context);

   function Has_Context
     (This    : Instance'Class;
      Context : As.Files.File_Context)
      return Boolean;

   function Get_Address
     (This    : Instance'Class;
      Context : As.Files.File_Context)
      return Word_32;

   type Context_Data is array (Positive range <>) of Word_8;

   function Get_Data
     (This    : Instance'Class;
      Context : As.Files.File_Context)
      return Context_Data;

   procedure Append
     (This : in out Instance'Class;
      Value : Word_8);

   procedure Append
     (This : in out Instance'Class;
      Value : Word_16);

   procedure Append
     (This : in out Instance'Class;
      Value : Word_32);

   procedure Loader
     (This    : in out Instance'Class;
      X, Y, Z : Word_8);

   procedure Globals
     (This : in out Instance'Class;
      Last : Register_Index);

   procedure Global_Value
     (This  : in out Instance'Class;
      Value : Word_32);

   procedure Write
     (This : in out Instance'Class;
      Path : String);

private

   Loader_Value : constant Word_8 := 16#98#;

   package Word_8_Vectors is
     new Ada.Containers.Vectors (Positive, Word_8);

   type Context_Record is
      record
         Context : As.Files.File_Context;
         Address : Word_32;
         First   : Positive;
         Last    : Natural;
      end record;

   package Context_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Context_Record);

   type Instance is tagged
      record
         Data     : Word_8_Vectors.Vector;
         Contexts : Context_Lists.List;
         Current  : Context_Lists.Cursor;
         Raw      : Boolean := True;
         Loc      : Word_32 := 0;
      end record;

   function Location
     (This : Instance'Class)
      return Word_32
   is (This.Loc);

   function Has_Context
     (This    : Instance'Class;
      Context : As.Files.File_Context)
      return Boolean
   is (for some Element of This.Contexts =>
          As.Files."=" (Element.Context.File, Context.File)
       and then Element.Context.Line = Context.Line);

end As.Objects;
