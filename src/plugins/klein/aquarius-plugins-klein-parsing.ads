package Aquarius.Plugins.Klein.Parsing is

   procedure Block_Before
     (Target : not null access Actions.Actionable'Class);

   procedure Defining_Identifier_List_After
     (Target : not null access Actions.Actionable'Class);

   procedure Defining_Package_Name_After
     (Target : not null access Actions.Actionable'Class);

   procedure Package_Reference_After
     (Target : not null access Actions.Actionable'Class);

   procedure Package_Spec_After
     (Target : not null access Actions.Actionable'Class);

   procedure Procedure_Declaration_After
     (Target : not null access Actions.Actionable'Class);

   procedure Procedure_Definition_After
     (Target : not null access Actions.Actionable'Class);

   procedure Subprogram_After
     (Target : not null access Actions.Actionable'Class);

   procedure Program_Unit_Before
     (Target : not null access Actions.Actionable'Class);

   procedure Program_Unit_After
     (Target : not null access Actions.Actionable'Class);

   procedure Type_Declaration_After
     (Target : not null access Actions.Actionable'Class);
   procedure Variable_Declaration_After
     (Target : not null access Actions.Actionable'Class);

   procedure List_Of_Declarations_After
     (Target : not null access Actions.Actionable'Class);

   procedure Type_After
     (Target : not null access Actions.Actionable'Class);

   procedure Object_Statement_Before_Expression
     (Object_Actionable     : not null access Actions.Actionable'Class;
      Expression_Actionable : not null access Actions.Actionable'Class);

   procedure Object_Statement_After
     (Target : not null access Actions.Actionable'Class);

   procedure Until_Statement_Before
     (Target : not null access Actions.Actionable'Class);

   procedure Numeric_Literal_After
     (Target : not null access Actions.Actionable'Class);

   procedure Qualified_Identifier_After
     (Target : not null access Actions.Actionable'Class);
   procedure Named_Type_After
     (Target : not null access Actions.Actionable'Class);
   procedure Record_Type_Before
     (Target : not null access Actions.Actionable'Class);
   procedure Record_Type_Before_Component
     (Record_Type_Actionable : not null access Actions.Actionable'Class;
      Component_Actionable   : not null access Actions.Actionable'Class);
   procedure Record_Component_After
     (Target : not null access Actions.Actionable'Class);

end Aquarius.Plugins.Klein.Parsing;
