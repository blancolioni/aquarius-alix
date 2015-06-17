with Aqua.Execution;

package Aqua.Primitives is

   type Primitive_Handler is access
     function (Context : in out Aqua.Execution.Execution_Interface'Class;
               Arguments : Array_Of_Words)
               return Word;

   procedure New_Primitive
     (Name           : String;
      Argument_Count : Natural;
      Handler        : Primitive_Handler);

   function Call_Primitive
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Primitive : Subroutine_Reference)
      return Word;

   function Get_Primitive
     (Name : String)
      return Subroutine_Reference;

end Aqua.Primitives;
