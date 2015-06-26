with Ada.Containers.Vectors;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Text_IO;

with Aqua.IO;

package body Aqua.Primitives is

   type Primitive_Object_Info is
      record
         Name     : access String;
         Instance : Primitive_Object_Access;
      end record;

   package Primitive_Object_Vectors is
     new Ada.Containers.Vectors (Positive, Primitive_Object_Info);

   type Primitive_Function_Info is
      record
         Name      : access String;
         Arg_Count : Natural;
         Handler   : Primitive_Handler;
      end record;

   package Primitive_Function_Vectors is
     new Ada.Containers.Vectors (Positive, Primitive_Function_Info);

   Prim_Objects   : Primitive_Object_Vectors.Vector;
   Prim_Functions : Primitive_Function_Vectors.Vector;

   --------------------
   -- Call_Primitive --
   --------------------

   function Call_Primitive
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Primitive : Subroutine_Reference;
      Arguments : Array_Of_Words)
      return Word
   is
      Info : Primitive_Function_Info
      renames Prim_Functions (Positive (Primitive));
   begin
      return Info.Handler (Context, Arguments);
   end Call_Primitive;

   --------------------
   -- Call_Primitive --
   --------------------

   function Call_Primitive
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Primitive : Subroutine_Reference)
      return Word
   is
      Info : Primitive_Function_Info renames
               Prim_Functions (Positive (Primitive));
      Args : Array_Of_Words (1 .. Info.Arg_Count);
   begin
      for I in Args'Range loop
         Args (I) := Context.Pop;
      end loop;
      return Info.Handler (Context, Args);
   end Call_Primitive;

   -------------------
   -- Get_Primitive --
   -------------------

   function Get_Primitive
     (Name : String)
      return Subroutine_Reference
   is
   begin
      for I in 1 .. Prim_Functions.Last_Index loop
         if Ada.Strings.Equal_Case_Insensitive
           (Prim_Functions (I).Name.all, Name)
         then
            return Subroutine_Reference (I);
         end if;
      end loop;
      return 0;
   end Get_Primitive;

   ----------------------------
   -- Load_Primitive_Objects --
   ----------------------------

   procedure Load_Primitive_Objects
     (Executor : in out Aqua.Execution.Execution_Interface'Class)
   is
   begin
      for I in 1 .. Prim_Objects.Last_Index loop
         declare
            W : constant Word :=
                  Executor.To_Word (Prim_Objects (I).Instance);
         begin
            Ada.Text_IO.Put_Line
              (Prim_Objects (I).Name.all & ": "
               & Aqua.IO.Hex_Image (W)
               & " - " & Executor.Show (W));
         end;
      end loop;
   end Load_Primitive_Objects;

   ----------------------------
   -- New_Primitive_Function --
   ----------------------------

   procedure New_Primitive_Function
     (Name           : String;
      Argument_Count : Natural;
      Handler        : Primitive_Handler)
   is
   begin
      Prim_Functions.Append ((new String'(Name), Argument_Count, Handler));
   end New_Primitive_Function;

   --------------------------
   -- New_Primitive_Object --
   --------------------------

   procedure New_Primitive_Object
     (Name : String;
      Item : not null access External_Object_Interface'Class)
   is
   begin
      Prim_Objects.Append
        ((new String'(Name), Primitive_Object_Access (Item)));
   end New_Primitive_Object;

end Aqua.Primitives;
