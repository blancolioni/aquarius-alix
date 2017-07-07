package body Aquarius.Ack.Parser.Expressions is

   generic
      Prefix_Operators : String;
      Sub_Expressions  : String;
      Infix_Operators  : String;
      with function Import_Sub_Expression
        (Program : Aquarius.Programs.Program_Tree)
         return Node_Id;
   function Generic_Import
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id;

   function Import_Primary (From : Aquarius.Programs.Program_Tree)
                            return Node_Id;

   --------------------
   -- Generic_Import --
   --------------------

   function Generic_Import
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Children : constant Array_Of_Program_Trees := From.Direct_Children;
      Index    : Positive := Children'First;
      Prefix   : Program_Tree;
      Node     : Node_Id;
   begin
      if Children (Index).Name = Prefix_Operators then
         Prefix := Children (Index);
         Index := Index + 1;
      end if;

      pragma Assert (Index <= Children'Last);
      pragma Assert (Children (Index).Name = Sub_Expressions);

      Node := Import_Sub_Expression (Children (Index));
      Index := Index + 1;

      while Index <= Children'Last loop
         declare
            Operator : constant Program_Tree := Children (Index);
            Right    : constant Program_Tree := Children (Index + 1);
         begin
            pragma Assert (Operator.Name = Infix_Operators);
            pragma Assert (Right.Name = Sub_Expressions);

            Index := Index + 2;
            Node := New_Node (N_Operator, Operator,
                              Field_1 => Node,
                              Field_2 => Import_Sub_Expression (Right),
                              Name    =>
                                Get_Name_Id (Operator.Concatenate_Children));
         end;
      end loop;

      if Prefix /= null then
         Node := New_Node (N_Operator, From,
                           Field_1 => Node,
                           Name    =>
                             Get_Name_Id (Prefix.Concatenate_Children));
      end if;

      return Node;
   end Generic_Import;

   -----------------------
   -- Import_Expression --
   -----------------------

   function Import_Expression
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;

      function Import_Factor is
        new Generic_Import
          ("unary_primary_operator", "primary", "",
           Import_Primary);
      function Import_Term is
        new Generic_Import
          ("", "factor", "multiplying_operator",
           Import_Factor);

      function Import_Simple_Expression is
        new Generic_Import
          ("unary_adding_operator", "term",
           "binary_adding_operator",
           Import_Term);

      function Import_Relation is
        new Generic_Import
          ("", "simple_expression",
           "relational_operator",
           Import_Simple_Expression);

      function Import_Top_Level is
        new Generic_Import
          ("", "relation",
           "boolean_operator",
           Import_Relation);
   begin
      if From.Program_Child ("member_of") /= null then
         return New_Node (N_Operator, From,
                          Field_1 =>
                            Import_Relation
                              (From.Program_Child ("relation")),
                          Field_2 =>
                            Import_Class_Name
                              (From.Program_Child ("class_name")),
                          Name    =>
                            Get_Name_Id
                              (From.Program_Child ("member_of")
                               .Concatenate_Children));
      else
         return Import_Top_Level (From);
      end if;
   end Import_Expression;

   ------------------------------
   -- Import_Manifest_Constant --
   ------------------------------

   function Import_Manifest_Constant
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return New_Node (N_Constant, From,
                       Name => Get_Name_Id (From.Concatenate_Children));
   end Import_Manifest_Constant;

   ----------------------
   -- Import_Precursor --
   ----------------------

   function Import_Precursor
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Es : constant Array_Of_Program_Trees :=
             From.Direct_Children (Skip_Separators => True);
      List : constant List_Id := New_List;
   begin
      for E of Es loop
         Append (List,
                 New_Node
                   (N_Identifier, E,
                    Name =>
                      Get_Name_Id (E.Program_Child ("identifier").Text)));
      end loop;
      return New_Node (N_Precursor, From, List => List);
   end Import_Precursor;

   --------------------
   -- Import_Primary --
   --------------------

   function Import_Primary
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      Choice : constant Aquarius.Programs.Program_Tree :=
                 From.Chosen_Tree;
   begin
      if Choice.Name = "manifest_constant" then
         return Import_Manifest_Constant (Choice);
      elsif Choice.Name = "precursor" then
         return Import_Precursor (Choice);
      elsif Choice.First_Child.Text = "(" then
         return Import_Expression (Choice.Program_Child ("expression"));
      else
         return No_Node;
      end if;
   end Import_Primary;

end Aquarius.Ack.Parser.Expressions;
