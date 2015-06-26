with Aqua.Execution;

package Aqua.Primitives is

   procedure New_Primitive_Object
     (Name : String;
      Item : not null access External_Object_Interface'Class);

   procedure Load_Primitive_Objects
     (Executor : in out Aqua.Execution.Execution_Interface'Class);

   type Primitive_Handler is access
     function (Context : in out Aqua.Execution.Execution_Interface'Class;
               Arguments : Array_Of_Words)
               return Word;

   procedure New_Primitive_Function
     (Name           : String;
      Argument_Count : Natural;
      Handler        : Primitive_Handler);

   function Call_Primitive
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Primitive : Subroutine_Reference)
      return Word;

   function Call_Primitive
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Primitive : Subroutine_Reference;
      Arguments : Array_Of_Words)
      return Word;

   function Get_Primitive
     (Name : String)
      return Subroutine_Reference;

private

   type Primitive_Object_Access is access all External_Object_Interface'Class;

end Aqua.Primitives;
