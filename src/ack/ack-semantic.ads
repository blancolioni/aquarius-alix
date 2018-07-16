with Ack.Classes;

package Ack.Semantic is

   procedure Analyse_Class_Declaration
     (Node : Node_Id);

   function Get_Class
     (Qualified_Name : String)
      return Ack.Classes.Constant_Class_Entity;

private

   procedure Analyse_Feature_Header
     (Class   : Ack.Classes.Class_Entity;
      Feature : Node_Id)
     with Pre => Kind (Feature) = N_Feature_Declaration;

   procedure Analyse_Feature_Body
     (Class   : Ack.Classes.Class_Entity;
      Feature : Node_Id)
     with Pre => Kind (Feature) = N_Feature_Declaration;

end Ack.Semantic;
