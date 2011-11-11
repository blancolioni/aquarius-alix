with Ada.Strings.Unbounded;

with Aquarius.AST.Expressions;

with Aquarius.Code;
with Aquarius.Source;
with Aquarius.Target;

package Aquarius.AST.Atoms is

   function New_Value_Node (Position : Aquarius.Source.Source_Position;
                            Value    : Aquarius.Target.Aquarius_Value;
                            Size     : Aquarius.Target.Aquarius_Data_Size)
                           return Aquarius.AST.Expressions.Expression;


   function New_Frame_Node (Position : Aquarius.Source.Source_Position;
                            Frame    : Aquarius.Code.Frame_Reference;
                            Size     : Aquarius.Target.Aquarius_Data_Size)
                           return Aquarius.AST.Expressions.Expression;


   function New_Label_Node (Position : Aquarius.Source.Source_Position;
                            Label    : String;
                            Size     : Aquarius.Target.Aquarius_Data_Size)
                           return Aquarius.AST.Expressions.Expression;


private

   type Atom_Node is
     abstract new Aquarius.AST.Expressions.Expression_Node with
      record
         Size : Aquarius.Target.Aquarius_Data_Size;
      end record;

   procedure Create_Atom_Node
     (Node     :    out Atom_Node;
      Position : in     Aquarius.Source.Source_Position;
      Size     : in     Aquarius.Target.Aquarius_Data_Size);

   type Value_Node is new Atom_Node with
      record
         Value : Aquarius.Target.Aquarius_Value;
      end record;

   procedure Generate (Node : access Value_Node;
                       Gen  : in out Aquarius.Code.Generator'Class);

   type Frame_Node is new Atom_Node with
      record
         Frame : Aquarius.Code.Frame_Reference;
      end record;

   procedure Generate (Node : access Frame_Node;
                       Gen  : in out Aquarius.Code.Generator'Class);

   type Label_Node is new Atom_Node with
      record
         Label : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   procedure Generate (Node : access Label_Node;
                       Gen  : in out Aquarius.Code.Generator'Class);

end Aquarius.AST.Atoms;
