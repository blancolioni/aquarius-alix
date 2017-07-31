with Ack.Types;

package Ack.Generate.Primitives is

   function Generate_Operator
     (Unit      : in out Tagatha.Units.Tagatha_Unit;
      Operator  : Name_Id;
      Left_Type : Ack.Types.Type_Entity)
      return Boolean;

end Ack.Generate.Primitives;
