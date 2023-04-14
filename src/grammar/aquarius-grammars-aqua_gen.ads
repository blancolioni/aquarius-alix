package Aquarius.Grammars.Aqua_Gen is

   procedure Generate
     (Grammar : Aquarius_Grammar);

   procedure Generate_Ada_Binding
     (Grammar : Aquarius_Grammar;
      Path    : String);

   type Aqua_Generator_Type is tagged private;

   procedure Create
     (Generator : in out Aqua_Generator_Type'Class;
      Grammar   : Aquarius_Grammar;
      Path      : String);

   procedure Add_Ancestor_Class
     (Generator : in out Aqua_Generator_Type'Class;
      Root      : Aquarius.Syntax.Syntax_Tree;
      Group     : Aquarius.Actions.Action_Group;
      Name      : String);

   procedure Execute
     (Generator : Aqua_Generator_Type'Class);

private

   type Ancestor_Record is
      record
         Root  : Aquarius.Syntax.Syntax_Tree;
         Group : Aquarius.Actions.Action_Group;
         Name  : Aquarius.Names.Aquarius_Name;
      end record;

   package Ancestor_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Ancestor_Record);

   type Aqua_Generator_Type is tagged
      record
         Grammar   : Aquarius_Grammar;
         Ancestors : Ancestor_Lists.List;
         Path      : Aquarius.Names.Aquarius_Name;
      end record;

   procedure Generate_Empty_Class
     (Gen        : Aqua_Generator_Type'Class;
      Class_Name : String);

   procedure Generate_Binding
     (Gen        : Aqua_Generator_Type'Class;
      Group_Name : String;
      Rule       : Aquarius.Syntax.Syntax_Tree);

   procedure Generate_Bindings
     (Gen        : Aqua_Generator_Type'Class;
      Group_Name : String);

end Aquarius.Grammars.Aqua_Gen;
