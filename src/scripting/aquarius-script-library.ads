with Aquarius.Script.Expressions;

private package Aquarius.Script.Library is

   function Standard return Script_Environment;

   function Execute
     (Item : not null access Root_Aquarius_Object'Class;
      Env  : Script_Environment;
      Args : Aquarius.Script.Expressions.Array_Of_Expressions)
      return Aquarius.Script.Expressions.Expression_Access;

end Aquarius.Script.Library;
