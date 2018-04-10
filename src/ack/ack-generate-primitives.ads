with Ack.Classes;
with Ack.Types;

package Ack.Generate.Primitives is

   function Generate_Operator
     (Unit      : in out Tagatha.Units.Tagatha_Unit;
      Operator  : Name_Id;
      Left_Type : Ack.Types.Type_Entity)
      return Boolean;

   procedure Generate_Intrinsic
     (Unit : in out Tagatha.Units.Tagatha_Unit;
      Name : Name_Id);

   procedure Create_Integral_Primitives
     (For_Class : Ack.Classes.Class_Entity);

end Ack.Generate.Primitives;
