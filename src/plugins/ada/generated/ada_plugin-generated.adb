with Aquarius.Actions;
with Aquarius.Programs;
with Ada_Plugin.Names, Ada_Plugin.Names.Gen, Ada_Plugin.Ch02,
     Ada_Plugin.Ch03, Ada_Plugin.Ch04, Ada_Plugin.Ch04.Gen,
     Ada_Plugin.Ch05, Ada_Plugin.Ch05.Gen, Ada_Plugin.Ch06,
     Ada_Plugin.Ch07, Ada_Plugin.Ch10, Ada_Plugin.Ch10.Gen;
package body Ada_Plugin.Generated is
   pragma Style_Checks (Off);
   package Names is
      procedure Actionable_Defining_Qualified_Reference_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Qualified_Reference_After_Identifier (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                 Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Defining_Qualified_Reference_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Object_Reference_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Object_Reference_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Direct_Name_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Expanded_Name_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Attribute_Reference_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Actual_Argument_List_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
   end Names;
   package Names_Gen is
      procedure Actionable_Direct_Name_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Object_Reference_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
   end Names_Gen;
   package Ch02 is
      procedure Actionable_Numeric_Literal_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
   end Ch02;
   package Ch03 is
      procedure Actionable_Access_Type_Definition_After_Subtype_Mark (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                      Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Array_Type_Definition_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Component_Definition_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Derived_Type_Definition_After_Qualified_Reference (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                              Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Enumerated_Type_Definition_After_Enumerated_Literal (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                                Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Enumerated_Type_Definition_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Modular_Type_Definition_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Modular_Type_Definition_After_Static_Expression (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                            Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Interface_Type_Definition_After_Interface (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                      Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Private_Type_Definition_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Range_Type_Definition_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Record_Type_Definition_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Record_Type_Definition_Before_Component_Definition (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                               Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Function_Type_Definition_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Procedure_Type_Definition_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Subtype_Declaration_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Subtype_Indication_After_Subtype_Mark (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                  Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Subtype_Mark_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Type_Declaration_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Type_Declaration_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Type_Definition_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Type_Header_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Type_Indication_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Variable_Declaration_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Record_Component_List_Before_Variant_Record_Element (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                                Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Variant_Record_Element_Before_Variant_Case (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                       Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Variant_Case_Before_Record_Component_List (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                      Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Type_Declaration_Before_Type_Definition (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                    Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Block_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_List_Of_Declarations_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
   end Ch03;
   package Ch04 is
      procedure Actionable_Relation_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Simple_Expression_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Term_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Factor_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Factor_After_Not (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                             Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Primary_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Boolean_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Relational_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Binary_Adding_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Multiplying_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Exponentation_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Unary_Adding_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Unary_Primary_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Expression_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
   end Ch04;
   package Ch04_Gen is
      procedure Actionable_Expression_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Primary_After_Numeric_Literal (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                          Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Primary_After_Object_Reference (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                           Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Relation_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Simple_Expression_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Term_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Factor_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Exponentation_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Multiplying_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Binary_Adding_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Relational_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Boolean_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
   end Ch04_Gen;
   package Ch05 is
      procedure Actionable_Procedure_Call_Before_Object_Reference (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                   Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Assignment_Statement_Before_Expression (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                   Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Assignment_Statement_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_While_Statement_Header_Before_Expression (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                     Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
   end Ch05;
   package Ch05_Gen is
      procedure Actionable_Assignment_Statement_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_While_Statement_Header_After_Expression (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                    Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_While_Statement_Header_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Loop_Statement_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Loop_Statement_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Statement_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
   end Ch05_Gen;
   package Ch06 is
      procedure Actionable_Procedure_Specification_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Function_Specification_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Procedure_Specification_After_Defining_Program_Unit_Name (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                                     Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Function_Specification_After_Defining_Program_Unit_Name (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                                    Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Function_Specification_Before_Formal_Argument_Spec (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                               Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Procedure_Specification_Before_Formal_Argument_Spec (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                                Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Formal_Argument_Spec_Before_Formal_Argument (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                        Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Formal_Argument_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Function_Specification_After_Type_Indication (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                         Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Procedure_Declaration_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
   end Ch06;
   package Ch07 is
      procedure Actionable_Package_Spec_After_Defining_Package_Name (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                     Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
   end Ch07;
   package Ch10 is
      procedure Actionable_Compilation_Unit_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_With_Clause_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
   end Ch10;
   package Ch10_Gen is
      procedure Actionable_Compilation_Unit_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Compilation_Unit_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
   end Ch10_Gen;
   package body Names is
      procedure Actionable_Defining_Qualified_Reference_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Names.Defining_Qualified_Reference_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Defining_Qualified_Reference_After;
      procedure Actionable_Qualified_Reference_After_Identifier (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                 Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Names.Qualified_Reference_After_Identifier (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Qualified_Reference_After_Identifier;
      procedure Actionable_Defining_Qualified_Reference_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Names.Defining_Qualified_Reference_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Defining_Qualified_Reference_Before;
      procedure Actionable_Object_Reference_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Names.Object_Reference_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Object_Reference_Before;
      procedure Actionable_Object_Reference_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Names.Object_Reference_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Object_Reference_After;
      procedure Actionable_Direct_Name_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Names.Direct_Name_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Direct_Name_After;
      procedure Actionable_Expanded_Name_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Names.Expanded_Name_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Expanded_Name_After;
      procedure Actionable_Attribute_Reference_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Names.Attribute_Reference_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Attribute_Reference_After;
      procedure Actionable_Actual_Argument_List_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Names.Actual_Argument_List_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Actual_Argument_List_After;
   end Names;
   package body Names_Gen is
      procedure Actionable_Direct_Name_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Names.Gen.Direct_Name_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Direct_Name_After;
      procedure Actionable_Object_Reference_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Names.Gen.Object_Reference_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Object_Reference_After;
   end Names_Gen;
   package body Ch02 is
      procedure Actionable_Numeric_Literal_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch02.Numeric_Literal_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Numeric_Literal_After;
   end Ch02;
   package body Ch03 is
      procedure Actionable_Access_Type_Definition_After_Subtype_Mark (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                      Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Access_Type_After_Subtype_Mark (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Access_Type_Definition_After_Subtype_Mark;
      procedure Actionable_Array_Type_Definition_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Array_Type_Definition_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Array_Type_Definition_Before;
      procedure Actionable_Component_Definition_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Component_Definition_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Component_Definition_After;
      procedure Actionable_Derived_Type_Definition_After_Qualified_Reference (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                              Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Derived_Type_After_Qualified_Reference (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Derived_Type_Definition_After_Qualified_Reference;
      procedure Actionable_Enumerated_Type_Definition_After_Enumerated_Literal (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                                Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Enumerated_Type_Definition_After_Enumerated_Literal (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Enumerated_Type_Definition_After_Enumerated_Literal;
      procedure Actionable_Enumerated_Type_Definition_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Enumerated_Type_Definition_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Enumerated_Type_Definition_Before;
      procedure Actionable_Modular_Type_Definition_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Modular_Type_Definition_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Modular_Type_Definition_Before;
      procedure Actionable_Modular_Type_Definition_After_Static_Expression (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                            Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Modular_Type_Definition_After_Static_Expression (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Modular_Type_Definition_After_Static_Expression;
      procedure Actionable_Interface_Type_Definition_After_Interface (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                      Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Interface_Type_After_Interface (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Interface_Type_Definition_After_Interface;
      procedure Actionable_Private_Type_Definition_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Private_Type_Definition_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Private_Type_Definition_Before;
      procedure Actionable_Range_Type_Definition_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Range_Type_Definition_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Range_Type_Definition_After;
      procedure Actionable_Record_Type_Definition_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Record_Type_Definition_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Record_Type_Definition_Before;
      procedure Actionable_Record_Type_Definition_Before_Component_Definition (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                               Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Record_Type_Definition_Before_Component (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Record_Type_Definition_Before_Component_Definition;
      procedure Actionable_Function_Type_Definition_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Subprogram_Type_Definition_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Function_Type_Definition_Before;
      procedure Actionable_Procedure_Type_Definition_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Subprogram_Type_Definition_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Procedure_Type_Definition_Before;
      procedure Actionable_Subtype_Declaration_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Subtype_Declaration_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Subtype_Declaration_After;
      procedure Actionable_Subtype_Indication_After_Subtype_Mark (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                  Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Subtype_Indication_After_Subtype_Mark (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Subtype_Indication_After_Subtype_Mark;
      procedure Actionable_Subtype_Mark_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Subtype_Mark_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Subtype_Mark_After;
      procedure Actionable_Type_Declaration_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Type_Declaration_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Type_Declaration_After;
      procedure Actionable_Type_Declaration_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Type_Declaration_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Type_Declaration_Before;
      procedure Actionable_Type_Definition_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Type_Definition_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Type_Definition_After;
      procedure Actionable_Type_Header_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Type_Header_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Type_Header_After;
      procedure Actionable_Type_Indication_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Type_Indication_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Type_Indication_After;
      procedure Actionable_Variable_Declaration_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Variable_Declaration_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Variable_Declaration_After;
      procedure Actionable_Record_Component_List_Before_Variant_Record_Element (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                                Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Transfer_Type (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Record_Component_List_Before_Variant_Record_Element;
      procedure Actionable_Variant_Record_Element_Before_Variant_Case (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                       Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Transfer_Type (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Variant_Record_Element_Before_Variant_Case;
      procedure Actionable_Variant_Case_Before_Record_Component_List (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                      Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Transfer_Type (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Variant_Case_Before_Record_Component_List;
      procedure Actionable_Type_Declaration_Before_Type_Definition (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                    Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Transfer_Entry (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Type_Declaration_Before_Type_Definition;
      procedure Actionable_Block_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.Block_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Block_Before;
      procedure Actionable_List_Of_Declarations_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch03.List_Of_Declarations_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_List_Of_Declarations_After;
   end Ch03;
   package body Ch04 is
      procedure Actionable_Relation_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Sub_Expression_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Relation_After;
      procedure Actionable_Simple_Expression_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Sub_Expression_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Simple_Expression_After;
      procedure Actionable_Term_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Sub_Expression_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Term_After;
      procedure Actionable_Factor_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Sub_Expression_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Factor_After;
      procedure Actionable_Factor_After_Not (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                             Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Factor_After_Not (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Factor_After_Not;
      procedure Actionable_Primary_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Primary_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Primary_After;
      procedure Actionable_Boolean_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Binary_Operator_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Boolean_Operator_After;
      procedure Actionable_Relational_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Binary_Operator_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Relational_Operator_After;
      procedure Actionable_Binary_Adding_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Binary_Operator_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Binary_Adding_Operator_After;
      procedure Actionable_Multiplying_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Binary_Operator_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Multiplying_Operator_After;
      procedure Actionable_Exponentation_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Binary_Operator_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Exponentation_Operator_After;
      procedure Actionable_Unary_Adding_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Unary_Operator_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Unary_Adding_Operator_After;
      procedure Actionable_Unary_Primary_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Unary_Operator_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Unary_Primary_Operator_After;
      procedure Actionable_Expression_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Expression_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Expression_After;
   end Ch04;
   package body Ch04_Gen is
      procedure Actionable_Expression_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Gen.Sub_Expression_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Expression_After;
      procedure Actionable_Primary_After_Numeric_Literal (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                          Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Gen.Primary_After_Numeric_Literal (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Primary_After_Numeric_Literal;
      procedure Actionable_Primary_After_Object_Reference (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                           Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Gen.Primary_After_Object_Reference (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Primary_After_Object_Reference;
      procedure Actionable_Relation_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Gen.Sub_Expression_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Relation_After;
      procedure Actionable_Simple_Expression_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Gen.Sub_Expression_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Simple_Expression_After;
      procedure Actionable_Term_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Gen.Sub_Expression_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Term_After;
      procedure Actionable_Factor_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Gen.Sub_Expression_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Factor_After;
      procedure Actionable_Exponentation_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Gen.Operator_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Exponentation_Operator_After;
      procedure Actionable_Multiplying_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Gen.Operator_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Multiplying_Operator_After;
      procedure Actionable_Binary_Adding_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Gen.Operator_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Binary_Adding_Operator_After;
      procedure Actionable_Relational_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Gen.Operator_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Relational_Operator_After;
      procedure Actionable_Boolean_Operator_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch04.Gen.Operator_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Boolean_Operator_After;
   end Ch04_Gen;
   package body Ch05 is
      procedure Actionable_Procedure_Call_Before_Object_Reference (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                   Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch05.Procedure_Call_Before_Object_Reference (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Procedure_Call_Before_Object_Reference;
      procedure Actionable_Assignment_Statement_Before_Expression (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                   Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch05.Assignment_Before_Expression (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Assignment_Statement_Before_Expression;
      procedure Actionable_Assignment_Statement_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch05.Assignment_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Assignment_Statement_After;
      procedure Actionable_While_Statement_Header_Before_Expression (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                     Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch05.Boolean_Expression_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_While_Statement_Header_Before_Expression;
   end Ch05;
   package body Ch05_Gen is
      procedure Actionable_Assignment_Statement_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch05.Gen.Assignment_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Assignment_Statement_After;
      procedure Actionable_While_Statement_Header_After_Expression (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                    Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch05.Gen.Transfer_Tagatha_Fragment (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_While_Statement_Header_After_Expression;
      procedure Actionable_While_Statement_Header_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch05.Gen.While_Statement_Header_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_While_Statement_Header_After;
      procedure Actionable_Loop_Statement_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch05.Gen.Loop_Statement_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Loop_Statement_Before;
      procedure Actionable_Loop_Statement_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch05.Gen.Loop_Statement_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Loop_Statement_After;
      procedure Actionable_Statement_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch05.Gen.Statement_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Statement_After;
   end Ch05_Gen;
   package body Ch06 is
      procedure Actionable_Procedure_Specification_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch06.Procedure_Specification_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Procedure_Specification_Before;
      procedure Actionable_Function_Specification_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch06.Function_Specification_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Function_Specification_Before;
      procedure Actionable_Procedure_Specification_After_Defining_Program_Unit_Name (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                                     Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch06.Procedure_Spec_After_Defining_Name (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Procedure_Specification_After_Defining_Program_Unit_Name;
      procedure Actionable_Function_Specification_After_Defining_Program_Unit_Name (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                                    Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch06.Procedure_Spec_After_Defining_Name (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Function_Specification_After_Defining_Program_Unit_Name;
      procedure Actionable_Function_Specification_Before_Formal_Argument_Spec (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                               Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch06.Spec_Before_Arguments (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Function_Specification_Before_Formal_Argument_Spec;
      procedure Actionable_Procedure_Specification_Before_Formal_Argument_Spec (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                                Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch06.Spec_Before_Arguments (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Procedure_Specification_Before_Formal_Argument_Spec;
      procedure Actionable_Formal_Argument_Spec_Before_Formal_Argument (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                        Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch06.Spec_Before_Arguments (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Formal_Argument_Spec_Before_Formal_Argument;
      procedure Actionable_Formal_Argument_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch06.Formal_Argument_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Formal_Argument_After;
      procedure Actionable_Function_Specification_After_Type_Indication (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                         Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch06.Function_Specification_After_Type_Indication (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Function_Specification_After_Type_Indication;
      procedure Actionable_Procedure_Declaration_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch06.Procedure_Declaration_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Procedure_Declaration_After;
   end Ch06;
   package body Ch07 is
      procedure Actionable_Package_Spec_After_Defining_Package_Name (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                     Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch07.Package_Spec_After_Defining_Package_Name (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Package_Spec_After_Defining_Package_Name;
   end Ch07;
   package body Ch10 is
      procedure Actionable_Compilation_Unit_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch10.Compilation_Unit_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Compilation_Unit_Before;
      procedure Actionable_With_Clause_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch10.With_Clause_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_With_Clause_After;
   end Ch10;
   package body Ch10_Gen is
      procedure Actionable_Compilation_Unit_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch10.Gen.Compilation_Unit_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Compilation_Unit_Before;
      procedure Actionable_Compilation_Unit_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Ada_Plugin.Ch10.Gen.Compilation_Unit_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_Compilation_Unit_After;
   end Ch10_Gen;
   procedure Bind_Actions (Plugin : in out Ada_Plugin_Type;
                           Grammar : in Aquarius.Grammars.Aquarius_Grammar) is
      Check : Aquarius.Actions.Action_Group;
      Generator : Aquarius.Actions.Action_Group;
   begin
      Grammar.Add_Action_Group ("check",
                                Aquarius.Actions.Semantic_Trigger,
                                Check);
      Grammar.Add_Action_Group ("generator",
                                Aquarius.Actions.Code_Trigger,
                                Generator);
      Plugin.Register_Action ("defining_qualified_reference", Check,
                              Aquarius.After,
                              Names.Actionable_Defining_Qualified_Reference_After'access);
      Plugin.Register_Action ("qualified_reference", "identifier",
                              Check, Aquarius.After,
                              Names.Actionable_Qualified_Reference_After_Identifier'access);
      Plugin.Register_Action ("defining_qualified_reference", Check,
                              Aquarius.Before,
                              Names.Actionable_Defining_Qualified_Reference_Before'access);
      Plugin.Register_Action ("object_reference", Check,
                              Aquarius.Before,
                              Names.Actionable_Object_Reference_Before'access);
      Plugin.Register_Action ("object_reference", Check,
                              Aquarius.After,
                              Names.Actionable_Object_Reference_After'access);
      Plugin.Register_Action ("direct_name", Check, Aquarius.After,
                              Names.Actionable_Direct_Name_After'access);
      Plugin.Register_Action ("expanded_name", Check, Aquarius.After,
                              Names.Actionable_Expanded_Name_After'access);
      Plugin.Register_Action ("attribute_reference", Check,
                              Aquarius.After,
                              Names.Actionable_Attribute_Reference_After'access);
      Plugin.Register_Action ("actual_argument_list", Check,
                              Aquarius.After,
                              Names.Actionable_Actual_Argument_List_After'access);
      Plugin.Register_Action ("direct_name", Generator, Aquarius.After,
                              Names_Gen.Actionable_Direct_Name_After'access);
      Plugin.Register_Action ("object_reference", Generator,
                              Aquarius.After,
                              Names_Gen.Actionable_Object_Reference_After'access);
      Plugin.Register_Action ("numeric_literal", Check, Aquarius.After,
                              Ch02.Actionable_Numeric_Literal_After'access);
      Plugin.Register_Action ("access_type_definition", "subtype_mark",
                              Check, Aquarius.After,
                              Ch03.Actionable_Access_Type_Definition_After_Subtype_Mark'access);
      Plugin.Register_Action ("array_type_definition", Check,
                              Aquarius.Before,
                              Ch03.Actionable_Array_Type_Definition_Before'access);
      Plugin.Register_Action ("component_definition", Check,
                              Aquarius.After,
                              Ch03.Actionable_Component_Definition_After'access);
      Plugin.Register_Action ("derived_type_definition",
                              "qualified_reference", Check,
                              Aquarius.After,
                              Ch03.Actionable_Derived_Type_Definition_After_Qualified_Reference'access);
      Plugin.Register_Action ("enumerated_type_definition",
                              "enumerated_literal", Check,
                              Aquarius.After,
                              Ch03.Actionable_Enumerated_Type_Definition_After_Enumerated_Literal'access);
      Plugin.Register_Action ("enumerated_type_definition", Check,
                              Aquarius.Before,
                              Ch03.Actionable_Enumerated_Type_Definition_Before'access);
      Plugin.Register_Action ("modular_type_definition", Check,
                              Aquarius.Before,
                              Ch03.Actionable_Modular_Type_Definition_Before'access);
      Plugin.Register_Action ("modular_type_definition",
                              "static_expression", Check,
                              Aquarius.After,
                              Ch03.Actionable_Modular_Type_Definition_After_Static_Expression'access);
      Plugin.Register_Action ("interface_type_definition", "interface",
                              Check, Aquarius.After,
                              Ch03.Actionable_Interface_Type_Definition_After_Interface'access);
      Plugin.Register_Action ("private_type_definition", Check,
                              Aquarius.Before,
                              Ch03.Actionable_Private_Type_Definition_Before'access);
      Plugin.Register_Action ("range_type_definition", Check,
                              Aquarius.After,
                              Ch03.Actionable_Range_Type_Definition_After'access);
      Plugin.Register_Action ("record_type_definition", Check,
                              Aquarius.Before,
                              Ch03.Actionable_Record_Type_Definition_Before'access);
      Plugin.Register_Action ("record_type_definition",
                              "component_definition", Check,
                              Aquarius.Before,
                              Ch03.Actionable_Record_Type_Definition_Before_Component_Definition'access);
      Plugin.Register_Action ("function_type_definition", Check,
                              Aquarius.Before,
                              Ch03.Actionable_Function_Type_Definition_Before'access);
      Plugin.Register_Action ("procedure_type_definition", Check,
                              Aquarius.Before,
                              Ch03.Actionable_Procedure_Type_Definition_Before'access);
      Plugin.Register_Action ("subtype_declaration", Check,
                              Aquarius.After,
                              Ch03.Actionable_Subtype_Declaration_After'access);
      Plugin.Register_Action ("subtype_indication", "subtype_mark",
                              Check, Aquarius.After,
                              Ch03.Actionable_Subtype_Indication_After_Subtype_Mark'access);
      Plugin.Register_Action ("subtype_mark", Check, Aquarius.After,
                              Ch03.Actionable_Subtype_Mark_After'access);
      Plugin.Register_Action ("type_declaration", Check,
                              Aquarius.After,
                              Ch03.Actionable_Type_Declaration_After'access);
      Plugin.Register_Action ("type_declaration", Check,
                              Aquarius.Before,
                              Ch03.Actionable_Type_Declaration_Before'access);
      Plugin.Register_Action ("type_definition", Check, Aquarius.After,
                              Ch03.Actionable_Type_Definition_After'access);
      Plugin.Register_Action ("type_header", Check, Aquarius.After,
                              Ch03.Actionable_Type_Header_After'access);
      Plugin.Register_Action ("type_indication", Check, Aquarius.After,
                              Ch03.Actionable_Type_Indication_After'access);
      Plugin.Register_Action ("variable_declaration", Check,
                              Aquarius.After,
                              Ch03.Actionable_Variable_Declaration_After'access);
      Plugin.Register_Action ("record_component_list",
                              "variant_record_element", Check,
                              Aquarius.Before,
                              Ch03.Actionable_Record_Component_List_Before_Variant_Record_Element'access);
      Plugin.Register_Action ("variant_record_element", "variant_case",
                              Check, Aquarius.Before,
                              Ch03.Actionable_Variant_Record_Element_Before_Variant_Case'access);
      Plugin.Register_Action ("variant_case", "record_component_list",
                              Check, Aquarius.Before,
                              Ch03.Actionable_Variant_Case_Before_Record_Component_List'access);
      Plugin.Register_Action ("type_declaration", "type_definition",
                              Check, Aquarius.Before,
                              Ch03.Actionable_Type_Declaration_Before_Type_Definition'access);
      Plugin.Register_Action ("block", Check, Aquarius.Before,
                              Ch03.Actionable_Block_Before'access);
      Plugin.Register_Action ("list_of_declarations", Check,
                              Aquarius.After,
                              Ch03.Actionable_List_Of_Declarations_After'access);
      Plugin.Register_Action ("relation", Check, Aquarius.After,
                              Ch04.Actionable_Relation_After'access);
      Plugin.Register_Action ("simple_expression", Check,
                              Aquarius.After,
                              Ch04.Actionable_Simple_Expression_After'access);
      Plugin.Register_Action ("term", Check, Aquarius.After,
                              Ch04.Actionable_Term_After'access);
      Plugin.Register_Action ("factor", Check, Aquarius.After,
                              Ch04.Actionable_Factor_After'access);
      Plugin.Register_Action ("factor", "not", Check, Aquarius.After,
                              Ch04.Actionable_Factor_After_Not'access);
      Plugin.Register_Action ("primary", Check, Aquarius.After,
                              Ch04.Actionable_Primary_After'access);
      Plugin.Register_Action ("boolean_operator", Check,
                              Aquarius.After,
                              Ch04.Actionable_Boolean_Operator_After'access);
      Plugin.Register_Action ("relational_operator", Check,
                              Aquarius.After,
                              Ch04.Actionable_Relational_Operator_After'access);
      Plugin.Register_Action ("binary_adding_operator", Check,
                              Aquarius.After,
                              Ch04.Actionable_Binary_Adding_Operator_After'access);
      Plugin.Register_Action ("multiplying_operator", Check,
                              Aquarius.After,
                              Ch04.Actionable_Multiplying_Operator_After'access);
      Plugin.Register_Action ("exponentation_operator", Check,
                              Aquarius.After,
                              Ch04.Actionable_Exponentation_Operator_After'access);
      Plugin.Register_Action ("unary_adding_operator", Check,
                              Aquarius.After,
                              Ch04.Actionable_Unary_Adding_Operator_After'access);
      Plugin.Register_Action ("unary_primary_operator", Check,
                              Aquarius.After,
                              Ch04.Actionable_Unary_Primary_Operator_After'access);
      Plugin.Register_Action ("expression", Check, Aquarius.After,
                              Ch04.Actionable_Expression_After'access);
      Plugin.Register_Action ("expression", Generator, Aquarius.After,
                              Ch04_Gen.Actionable_Expression_After'access);
      Plugin.Register_Action ("primary", "numeric_literal", Generator,
                              Aquarius.After,
                              Ch04_Gen.Actionable_Primary_After_Numeric_Literal'access);
      Plugin.Register_Action ("primary", "object_reference", Generator,
                              Aquarius.After,
                              Ch04_Gen.Actionable_Primary_After_Object_Reference'access);
      Plugin.Register_Action ("relation", Generator, Aquarius.After,
                              Ch04_Gen.Actionable_Relation_After'access);
      Plugin.Register_Action ("simple_expression", Generator,
                              Aquarius.After,
                              Ch04_Gen.Actionable_Simple_Expression_After'access);
      Plugin.Register_Action ("term", Generator, Aquarius.After,
                              Ch04_Gen.Actionable_Term_After'access);
      Plugin.Register_Action ("factor", Generator, Aquarius.After,
                              Ch04_Gen.Actionable_Factor_After'access);
      Plugin.Register_Action ("exponentation_operator", Generator,
                              Aquarius.After,
                              Ch04_Gen.Actionable_Exponentation_Operator_After'access);
      Plugin.Register_Action ("multiplying_operator", Generator,
                              Aquarius.After,
                              Ch04_Gen.Actionable_Multiplying_Operator_After'access);
      Plugin.Register_Action ("binary_adding_operator", Generator,
                              Aquarius.After,
                              Ch04_Gen.Actionable_Binary_Adding_Operator_After'access);
      Plugin.Register_Action ("relational_operator", Generator,
                              Aquarius.After,
                              Ch04_Gen.Actionable_Relational_Operator_After'access);
      Plugin.Register_Action ("boolean_operator", Generator,
                              Aquarius.After,
                              Ch04_Gen.Actionable_Boolean_Operator_After'access);
      Plugin.Register_Action ("procedure_call", "object_reference",
                              Check, Aquarius.Before,
                              Ch05.Actionable_Procedure_Call_Before_Object_Reference'access);
      Plugin.Register_Action ("assignment_statement", "expression",
                              Check, Aquarius.Before,
                              Ch05.Actionable_Assignment_Statement_Before_Expression'access);
      Plugin.Register_Action ("assignment_statement", Check,
                              Aquarius.After,
                              Ch05.Actionable_Assignment_Statement_After'access);
      Plugin.Register_Action ("while_statement_header", "expression",
                              Check, Aquarius.Before,
                              Ch05.Actionable_While_Statement_Header_Before_Expression'access);
      Plugin.Register_Action ("assignment_statement", Generator,
                              Aquarius.After,
                              Ch05_Gen.Actionable_Assignment_Statement_After'access);
      Plugin.Register_Action ("while_statement_header", "expression",
                              Generator, Aquarius.After,
                              Ch05_Gen.Actionable_While_Statement_Header_After_Expression'access);
      Plugin.Register_Action ("while_statement_header", Generator,
                              Aquarius.After,
                              Ch05_Gen.Actionable_While_Statement_Header_After'access);
      Plugin.Register_Action ("loop_statement", Generator,
                              Aquarius.Before,
                              Ch05_Gen.Actionable_Loop_Statement_Before'access);
      Plugin.Register_Action ("loop_statement", Generator,
                              Aquarius.After,
                              Ch05_Gen.Actionable_Loop_Statement_After'access);
      Plugin.Register_Action ("statement", Generator, Aquarius.After,
                              Ch05_Gen.Actionable_Statement_After'access);
      Plugin.Register_Action ("procedure_specification", Check,
                              Aquarius.Before,
                              Ch06.Actionable_Procedure_Specification_Before'access);
      Plugin.Register_Action ("function_specification", Check,
                              Aquarius.Before,
                              Ch06.Actionable_Function_Specification_Before'access);
      Plugin.Register_Action ("procedure_specification",
                              "defining_program_unit_name", Check,
                              Aquarius.After,
                              Ch06.Actionable_Procedure_Specification_After_Defining_Program_Unit_Name'access);
      Plugin.Register_Action ("function_specification",
                              "defining_program_unit_name", Check,
                              Aquarius.After,
                              Ch06.Actionable_Function_Specification_After_Defining_Program_Unit_Name'access);
      Plugin.Register_Action ("function_specification",
                              "formal_argument_spec", Check,
                              Aquarius.Before,
                              Ch06.Actionable_Function_Specification_Before_Formal_Argument_Spec'access);
      Plugin.Register_Action ("procedure_specification",
                              "formal_argument_spec", Check,
                              Aquarius.Before,
                              Ch06.Actionable_Procedure_Specification_Before_Formal_Argument_Spec'access);
      Plugin.Register_Action ("formal_argument_spec",
                              "formal_argument", Check,
                              Aquarius.Before,
                              Ch06.Actionable_Formal_Argument_Spec_Before_Formal_Argument'access);
      Plugin.Register_Action ("formal_argument", Check, Aquarius.After,
                              Ch06.Actionable_Formal_Argument_After'access);
      Plugin.Register_Action ("function_specification",
                              "type_indication", Check, Aquarius.After,
                              Ch06.Actionable_Function_Specification_After_Type_Indication'access);
      Plugin.Register_Action ("procedure_declaration", Check,
                              Aquarius.After,
                              Ch06.Actionable_Procedure_Declaration_After'access);
      Plugin.Register_Action ("package_spec", "defining_package_name",
                              Check, Aquarius.After,
                              Ch07.Actionable_Package_Spec_After_Defining_Package_Name'access);
      Plugin.Register_Action ("compilation_unit", Check,
                              Aquarius.Before,
                              Ch10.Actionable_Compilation_Unit_Before'access);
      Plugin.Register_Action ("with_clause", Check, Aquarius.After,
                              Ch10.Actionable_With_Clause_After'access);
      Plugin.Register_Action ("compilation_unit", Generator,
                              Aquarius.Before,
                              Ch10_Gen.Actionable_Compilation_Unit_Before'access);
      Plugin.Register_Action ("compilation_unit", Generator,
                              Aquarius.After,
                              Ch10_Gen.Actionable_Compilation_Unit_After'access);
   end Bind_Actions;
end Ada_Plugin.Generated;
