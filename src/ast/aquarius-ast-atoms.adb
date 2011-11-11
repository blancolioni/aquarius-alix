package body Aquarius.AST.Atoms is

   ----------------------
   -- Create_Atom_Node --
   ----------------------

   procedure Create_Atom_Node
     (Node     :    out Atom_Node;
      Position : in     Aquarius.Source.Source_Position;
      Size     : in     Aquarius.Target.Aquarius_Data_Size)
   is
   begin
      Create_Root_Node (Root_Tree_Node (Node), Position);
      Node.Size := Size;
   end Create_Atom_Node;

   --------------
   -- Generate --
   --------------

   procedure Generate (Node : access Frame_Node;
                       Gen  : in out Aquarius.Code.Generator'Class)
   is
   begin
      Gen.Push (Aquarius.Code.Frame_Argument (Node.Frame, Node.Size));
   end Generate;

   --------------
   -- Generate --
   --------------

   procedure Generate (Node : access Label_Node;
                       Gen  : in out Aquarius.Code.Generator'Class)
   is
   begin
      Gen.Push (Aquarius.Code.Label_Argument
                  (Ada.Strings.Unbounded.To_String (Node.Label),
                   Node.Size));
   end Generate;

   --------------
   -- Generate --
   --------------

   procedure Generate (Node : access Value_Node;
                       Gen  : in out Aquarius.Code.Generator'Class)
   is
   begin
      Gen.Push (Aquarius.Code.Value_Argument (Node.Value, Node.Size));
   end Generate;

   --------------------
   -- New_Frame_Node --
   --------------------

   function New_Frame_Node (Position : Aquarius.Source.Source_Position;
                            Frame    : Aquarius.Code.Frame_Reference;
                            Size     : Aquarius.Target.Aquarius_Data_Size)
                           return Aquarius.AST.Expressions.Expression
   is
      Result : Frame_Node;
   begin
      Create_Atom_Node (Result, Position, Size);
      Result.Frame := Frame;
      return new Frame_Node'(Result);
   end New_Frame_Node;

   --------------------
   -- New_Label_Node --
   --------------------

   function New_Label_Node (Position : Aquarius.Source.Source_Position;
                            Label    : String;
                            Size     : Aquarius.Target.Aquarius_Data_Size)
                           return Aquarius.AST.Expressions.Expression
   is
      Result : Label_Node;
   begin
      Create_Atom_Node (Result, Position, Size);
      Result.Label := Ada.Strings.Unbounded.To_Unbounded_String (Label);
      return new Label_Node'(Result);
   end New_Label_Node;


   --------------------
   -- New_Value_Node --
   --------------------

   function New_Value_Node (Position : Aquarius.Source.Source_Position;
                            Value    : Aquarius.Target.Aquarius_Value;
                            Size     : Aquarius.Target.Aquarius_Data_Size)
                           return Aquarius.AST.Expressions.Expression
   is
      Result : Value_Node;
   begin
      Create_Atom_Node (Result, Position, Size);
      Result.Value := Value;
      return new Value_Node'(Result);
   end New_Value_Node;

end Aquarius.AST.Atoms;
