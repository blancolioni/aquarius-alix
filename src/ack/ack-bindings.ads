with Aqua.Images;

with Aquarius.Actions;
with Aquarius.Grammars;

package Ack.Bindings is

   function Load_Ack_Binding
     (Action_File_Path : String;
      Base_Aqua_Path   : String;
      Image            : Aqua.Images.Image_Type;
      Grammar          : Aquarius.Grammars.Aquarius_Grammar;
      Group            : Aquarius.Actions.Action_Group;
      Trigger          : Aquarius.Actions.Action_Execution_Trigger)
      return Boolean;

private

   type Binding_Position is (Before, After);

end Ack.Bindings;
