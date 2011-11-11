with Aquarius.Entries.Objects;
with Aquarius.Entries.Packages;
with Aquarius.Entries.Types;
with Aquarius.Plugins.Klein.Expressions;
with Aquarius.Plugins.Klein.Names;
with Aquarius.Plugins.Klein.Parsing;
with Aquarius.Plugins.Klein.Types;
with Aquarius.Trees;
with Aquarius.Types.Maps;
with Aquarius.Values;

package body Aquarius.Plugins.Klein is

   Global_Klein_Plugin : Klein_Plugin_Access;

   procedure Declare_Standard_Operators
     (Table       : Aquarius.Entries.Symbol_Table;
      Type_Entry  : Aquarius.Entries.Table_Entry;
      For_Type    : Aquarius.Types.Aquarius_Type;
      Bool        : Aquarius.Types.Aquarius_Type);

   procedure Declare_Operator
     (Table       : Aquarius.Entries.Symbol_Table;
      Name        : String;
      Declaration : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Op_Type     : not null access Aquarius.Types.Root_Aquarius_Type'Class);

   ----------------------
   -- Declare_Operator --
   ----------------------

   procedure Declare_Operator
     (Table       : Aquarius.Entries.Symbol_Table;
      Name        : String;
      Declaration : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Op_Type     : not null access Aquarius.Types.Root_Aquarius_Type'Class)
   is
      Operator : constant Aquarius.Entries.Table_Entry :=
        Aquarius.Entries.Objects.New_Object_Entry
        (Name, Declaration, Op_Type,
         Aquarius.Values.No_Value, True);
   begin
      Table.Insert (Operator);
   end Declare_Operator;

   --------------------------------
   -- Declare_Standard_Operators --
   --------------------------------

   procedure Declare_Standard_Operators
     (Table       : Aquarius.Entries.Symbol_Table;
      Type_Entry  : Aquarius.Entries.Table_Entry;
      For_Type    : Aquarius.Types.Aquarius_Type;
      Bool        : Aquarius.Types.Aquarius_Type)
   is
      X_X_To_Boolean : constant Aquarius.Types.Aquarius_Type :=
        Aquarius.Types.Maps.New_Map_Type ((For_Type, For_Type), Bool);
      X_X_To_X       : constant Aquarius.Types.Aquarius_Type :=
        Aquarius.Types.Maps.New_Map_Type ((For_Type, For_Type), For_Type);
   begin
      Declare_Operator (Table, "=",  Type_Entry.Declaration, X_X_To_Boolean);
      Declare_Operator (Table, "/=", Type_Entry.Declaration, X_X_To_Boolean);
      Declare_Operator (Table, ">",  Type_Entry.Declaration, X_X_To_Boolean);
      Declare_Operator (Table, "<",  Type_Entry.Declaration, X_X_To_Boolean);
      Declare_Operator (Table, ">=", Type_Entry.Declaration, X_X_To_Boolean);
      Declare_Operator (Table, "<=", Type_Entry.Declaration, X_X_To_Boolean);

      Declare_Operator (Table, "+",   Type_Entry.Declaration, X_X_To_X);
      Declare_Operator (Table, "-",   Type_Entry.Declaration, X_X_To_X);
      Declare_Operator (Table, "*",   Type_Entry.Declaration, X_X_To_X);
      Declare_Operator (Table, "/",   Type_Entry.Declaration, X_X_To_X);
      Declare_Operator (Table, "mod", Type_Entry.Declaration, X_X_To_X);

   end Declare_Standard_Operators;

   ----------
   -- Load --
   ----------

   overriding
   procedure Load
     (Plugin  : not null access Klein_Plugin;
      Grammar : in Aquarius.Grammars.Aquarius_Grammar)
   is
      use Aquarius.Formats;
      Indented_Child_Format : constant Aquarius_Format :=
        Make_Format ((New_Line (Before), New_Line (After),
                      Indent (Before, 3), Indent (After, -3)));
      Parser   : Aquarius.Actions.Action_Group;
   begin

      Load (Aquarius_Plugin_Type (Plugin.all)'Access, Grammar);

      Grammar.Create_Property
        (Plugin.Property_Left, "klein-left", False, True);

      Grammar.Create_Property (Plugin.Property_Right, "klein-right",
                               False, True);
      Grammar.Create_Property (Plugin.Property_Function, "klein-function",
                               False, False);
      Grammar.Create_Property (Plugin.Property_Node, "klein-node",
                               False, False);
      Grammar.Create_Property (Plugin.Property_Expression,
                               "klein-expression", False, True);
      Grammar.Create_Property (Plugin.Property_Inferred_Types,
                               "klein-inferred-types", False, True);
      Grammar.Create_Property (Plugin.Property_Possible_Types,
                               "klein-possible-types", False, True);
      Grammar.Create_Property (Plugin.Object_Property, "object",
                               True, False);
      Grammar.Create_Property (Plugin.Package_Property, "package",
                               True, False);
      Grammar.Create_Property (Plugin.Record_Property, "record",
                               True, False);
      Grammar.Create_Property (Plugin.Procedure_Property, "procedure",
                               True, False);
      Grammar.Create_Property (Plugin.Package_Spec_Property, "spec",
                               True, False);
      Grammar.Create_Property (Plugin.Package_Body_Property, "body",
                               True, False);

      Grammar.Create_Property (Plugin.Type_Error_Tag, "type-error-tag",
                               False, False);

      Plugin.Register_Rule ("context_clause",
                            New_Line (After));
      Plugin.Register_Rule ("list_of_definitions",
                            Indented_Child_Format);
      Plugin.Register_Rule ("list_of_declarations",
                            Indented_Child_Format);
      Plugin.Register_Rule ("sequence_of_statements",
                            Indented_Child_Format);
      Plugin.Register_Rule ("record_type",
                            Make_Format
                              ((New_Line (Before),
                                Indent (Before, 3), Indent (After, -3))));
      Plugin.Register_Rule ("record_component_list",
                            Indented_Child_Format);
      Plugin.Register_Rule (";",
                            Make_Format ((Space_Never (Before),
                                          New_Line (After))));
      Plugin.Register_Rule ("(",
                            Make_Format ((Space (True, Before, 8),
                                          Space (False, After, 9))));

      Plugin.Register_Rule (")",
                            Make_Format ((Space (False, Before, 9),
                                          Space (False, After, 9))));

      Plugin.Register_Rule (",",
                            Space_Never (Before));

      Plugin.Register_Rule (".",
                            Space_Never (Before),
                            Space_Never (After));

      Grammar.Add_Action_Group ("parser",
                                Aquarius.Actions.Parse_Trigger,
                                Parser);

      Plugin.Register_Action
        ("program_unit", Parser, Before,
         Parsing.Program_Unit_Before'Access);

      Plugin.Register_Action
        ("program_unit", Parser, After,
         Parsing.Program_Unit_After'Access);

      Plugin.Register_Action
        ("subprogram", Parser, After,
         Parsing.Subprogram_After'Access);

      Plugin.Register_Action
        ("package_reference", Parser, After,
         Parsing.Package_Reference_After'Access);

      Plugin.Register_Action
        ("package_spec", Parser, After,
         Parsing.Package_Spec_After'Access);

      Plugin.Register_Action
        ("procedure_definition", Parser, After,
         Parsing.Procedure_Definition_After'Access);
      Plugin.Register_Action
        ("procedure_declaration", Parser, After,
         Parsing.Procedure_Declaration_After'Access);

      Plugin.Register_Action
        ("defining_package_name", Parser, After,
         Parsing.Defining_Package_Name_After'Access);

      Plugin.Register_Action
        ("block", Parser, Before,
         Parsing.Block_Before'Access);

      Plugin.Register_Action
        ("defining_identifier_list", Parser, After,
         Parsing.Defining_Identifier_List_After'Access);

      Plugin.Register_Action
        ("until_statement", Parser, Before,
         Parsing.Until_Statement_Before'Access);

      Plugin.Register_Action
        ("object_statement", "expression", Parser, Before,
         Parsing.Object_Statement_Before_Expression'Access);

      Plugin.Register_Action
        ("object_statement", Parser, After,
         Parsing.Object_Statement_After'Access);

      Plugin.Register_Action
        ("name", Parser, After,
         Names.Name_After'Access);

--        Plugin.Register_Action
--          ("name", "direct_name", Parser, After,
--           Names.Name_After_Direct_Name'Access);

--        Plugin.Register_Action
--          ("name", "name_qualifier", Parser, Before,
--           Names.Name_Before_Name_Qualifier'Access);
--        Plugin.Register_Action
--          ("name", "name_qualifier", Parser, After,
--           Names.Name_After_Name_Qualifier'Access);

      Plugin.Register_Action
        ("direct_name", Parser, After,
         Names.Direct_Name_After'Access);

--        Plugin.Register_Action
--          ("name_qualifier", Parser, After,
--           Names.Name_Qualifier_After'Access);

      Plugin.Register_Action
        ("component_name", Parser, After,
         Names.Component_Name_After'Access);
      Plugin.Register_Action
        ("expanded_name", Parser, After,
         Names.Expanded_Name_After'Access);

      Plugin.Register_Action
        ("list_of_declarations", Parser, After,
         Parsing.List_Of_Declarations_After'Access);
      Plugin.Register_Action
        ("qualified_identifier", Parser, After,
         Parsing.Qualified_Identifier_After'Access);
      Plugin.Register_Action
        ("named_type", Parser, After,
         Parsing.Named_Type_After'Access);
      Plugin.Register_Action
        ("type_definition", Parser, After,
         Parsing.Type_After'Access);
      Plugin.Register_Action
        ("type_declaration", Parser, After,
         Parsing.Type_Declaration_After'Access);
      Plugin.Register_Action
        ("record_type", Parser, Before,
         Parsing.Record_Type_Before'Access);
      Plugin.Register_Action
        ("record_type", "component_definition", Parser, Before,
         Parsing.Record_Type_Before_Component'Access);
      Plugin.Register_Action
        ("component_definition", Parser, After,
         Parsing.Record_Component_After'Access);

      Plugin.Register_Action
        ("variable_declaration", Parser, After,
         Parsing.Variable_Declaration_After'Access);

      Plugin.Register_Action
        ("expression", Parser, After,
         Expressions.Sub_Expression_After'Access);

      Plugin.Register_Action
        ("relation", Parser, After,
         Expressions.Sub_Expression_After'Access);

      Plugin.Register_Action
        ("simple_expression", Parser, After,
         Expressions.Sub_Expression_After'Access);

      Plugin.Register_Action
        ("term", Parser, After,
         Expressions.Sub_Expression_After'Access);

      Plugin.Register_Action
        ("factor", Parser, After,
         Expressions.Sub_Expression_After'Access);

      Plugin.Register_Action
        ("primary", Parser, After,
         Expressions.Primary_After'Access);

      Plugin.Register_Action
        ("boolean_operator", Parser, After,
         Expressions.Binary_Operator_After'Access);

      Plugin.Register_Action
        ("relational_operator", Parser, After,
         Expressions.Binary_Operator_After'Access);

      Plugin.Register_Action
        ("binary_adding_operator", Parser, After,
         Expressions.Binary_Operator_After'Access);

      Plugin.Register_Action
        ("multiplying_operator", Parser, After,
         Expressions.Binary_Operator_After'Access);

      Plugin.Register_Action
        ("unary_adding_operator", Parser, After,
         Expressions.Unary_Operator_After'Access);

      Plugin.Register_Action
        ("expression", Parser, After,
         Expressions.Expression_After'Access);

      Plugin.Register_Action
        ("numeric_literal", Parser, After,
         Parsing.Numeric_Literal_After'Access);

      Plugin.Load_Package_Standard;

      Global_Klein_Plugin := Klein_Plugin_Access (Plugin);

   end Load;

   ---------------------------
   -- Load_Package_Standard --
   ---------------------------

   procedure Load_Package_Standard (Plugin : in out Klein_Plugin) is
      Root_Integer_Type : Aquarius.Types.Aquarius_Type;
      Integer_Entry     : Aquarius.Entries.Table_Entry;
      Boolean_Entry     : Aquarius.Entries.Table_Entry;
      Standard_Table    : Aquarius.Entries.Symbol_Table;
      Package_Standard  : Aquarius.Entries.Table_Entry;
   begin
      Root_Integer_Type :=
        Plugins.Klein.Types.Create_Integer_Type (Integer'First,
                                                 Integer'Last);
      Root_Integer_Type :=
        Aquarius.Types.New_Named_Type ("Integer",
                                       Root_Integer_Type);
      Integer_Entry :=
        Aquarius.Entries.Types.New_Type_Entry
        ("integer", null, Root_Integer_Type);
      Boolean_Entry :=
        Aquarius.Entries.Types.New_Type_Entry
        ("boolean", null, Types.Universal_Boolean);
      Aquarius.Entries.Set_Display_Name (Integer_Entry, "Integer");
      Aquarius.Entries.Set_Display_Name (Boolean_Entry, "Boolean");

      Standard_Table := Aquarius.Entries.New_Symbol_Table ("standard");
      Standard_Table.Insert (Integer_Entry);
      Standard_Table.Insert (Boolean_Entry);

      Declare_Standard_Operators (Standard_Table,
                                  Integer_Entry,
                                  Root_Integer_Type,
                                  Types.Universal_Boolean);

      Package_Standard :=
        Aquarius.Entries.Packages.New_Standard_Package
        ("standard", Standard_Table);

      Plugin.Add_Standard_Entry (Package_Standard);

      Plugin.Error_Type :=
        Aquarius.Plugins.Klein.Types.New_Error_Type (null);

   end Load_Package_Standard;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Plugin : Klein_Plugin) return String is
      pragma Unreferenced (Plugin);
   begin
      return "Klein";
   end Name;

   ------------
   -- Plugin --
   ------------

   function Plugin return Klein_Plugin_Access is
   begin
      return Global_Klein_Plugin;
   end Plugin;

   -------------
   -- Version --
   -------------

   overriding
   function Version (Plugin : Klein_Plugin) return String is
      pragma Unreferenced (Plugin);
   begin
      return "0.1";
   end Version;

end Aquarius.Plugins.Klein;
