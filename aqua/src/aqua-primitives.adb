with Ada.Containers.Vectors;
with Ada.Strings.Equal_Case_Insensitive;

package body Aqua.Primitives is

   type Primitive_Info is
      record
         Name      : access String;
         Arg_Count : Natural;
         Handler   : Primitive_Handler;
      end record;

   package Primitive_Vectors is
     new Ada.Containers.Vectors (Positive, Primitive_Info);

   Prims : Primitive_Vectors.Vector;

   --------------------
   -- Call_Primitive --
   --------------------

   function Call_Primitive
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Primitive : Subroutine_Reference)
      return Word
   is
      Info : Primitive_Info renames Prims (Positive (Primitive));
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
      for I in 1 .. Prims.Last_Index loop
         if Ada.Strings.Equal_Case_Insensitive (Prims (I).Name.all, Name) then
            return Subroutine_Reference (I);
         end if;
      end loop;
      return 0;
   end Get_Primitive;

   -------------------
   -- New_Primitive --
   -------------------

   procedure New_Primitive
     (Name           : String;
      Argument_Count : Natural;
      Handler        : Primitive_Handler)
   is
   begin
      Prims.Append ((new String'(Name), Argument_Count, Handler));
   end New_Primitive;

end Aqua.Primitives;
