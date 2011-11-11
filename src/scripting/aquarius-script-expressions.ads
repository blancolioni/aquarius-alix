package Aquarius.Script.Expressions is

   type Expression_Element is
     abstract new Root_Script_Element with private;

   function Evaluate
     (Item        : Expression_Element;
      Environment : Script_Environment)
     return access Root_Aquarius_Object'Class
      is abstract;

   type Expression_Access is access all Expression_Element'Class;

   function Let_Expression
     (Name    : String;
      Binding : not null access Root_Script_Element'Class)
      return Expression_Access;

   function With_Expression
     (Withed_Item : String;
      Withed_Body : not null access Root_Script_Element'Class)
      return Expression_Access;

   function Identifier_Expression (Name : String) return Expression_Access;
   function String_Expression (Value : String) return Expression_Access;

   type Array_Of_Expressions is array (Positive range <>) of Expression_Access;

   function Call_Expression (Callee    : String;
                             Arguments : Array_Of_Expressions)
                             return Expression_Access;

private

   type Expression_Element is
     abstract new Root_Script_Element with null record;

   overriding
   procedure Execute
     (Item        : in     Expression_Element;
      Environment : in out Script_Environment);

end Aquarius.Script.Expressions;
