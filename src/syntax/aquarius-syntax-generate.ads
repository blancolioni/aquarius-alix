private with Ada.Containers.Doubly_Linked_Lists;

package Aquarius.Syntax.Generate is

   type Plugin_Generator_Type is tagged private;

   procedure Create
     (Generator : in out Plugin_Generator_Type'Class;
      Syntax    : Syntax_Tree);

   procedure Add_Ancestor_Class
     (Generator : in out Plugin_Generator_Type'Class;
      Root      : Syntax_Tree;
      Name      : String);

private

   type Ancestor_Record is
      record
         Root : Syntax_Tree;
         Name : Aquarius.Names.Aquarius_Name;
      end record;

   package Ancestor_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Ancestor_Record);

   type Plugin_Generator_Type is tagged
      record
         Syntax : Syntax_Tree;
         Ancestors : Ancestor_Lists.List;
      end record;

end Aquarius.Syntax.Generate;
