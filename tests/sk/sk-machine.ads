private with SK.Cells;

package SK.Machine is

   type SK_Machine is private;

   function Create_Machine (Size : Natural) return SK_Machine;
   function Load_Machine (Path : String) return SK_Machine;

   procedure Load_Library (M    : SK_Machine;
                           Path : String);

   function Start (M : SK_Machine) return Object;

   function Evaluate (M : SK_Machine;
                      E : Object)
                      return Object;

   procedure Evaluate (M : SK_Machine);
   --  Evaluates top of stack

   function Parse_String (M    : SK_Machine;
                          Text : String)
                          return Object;

   function Compile (M : SK_Machine;
                     E : Object)
                     return Object;

   function Show (M    : SK_Machine;
                  Item : Object)
                  return String;

   function Show_Stack_Top (M    : SK_Machine)
                           return String;

   procedure Bind (M : SK_Machine;
                   Name : String);
   --  bind object at top of stack to given name

   function Low_Level_Show (M    : SK_Machine;
                            Item : Object)
                            return String;

private

   type SK_Machine_Record;
   type SK_Machine is access SK_Machine_Record;

   function Get_Cells (M : SK_Machine) return SK.Cells.Managed_Cells;

end SK.Machine;
