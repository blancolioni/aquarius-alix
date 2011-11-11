with Aquarius.Grammars;
with Aquarius.Programs;

package Aquarius.Drys is

   type Program_Builder is abstract tagged
      record
         Tree : Aquarius.Programs.Program_Tree;
      end record;

   function Make_Tree (Item : Program_Builder'Class)
                       return Aquarius.Programs.Program_Tree;

   type Package_Spec_Type is new Program_Builder with null record;

   type Defining_Package_Name_Type is new Program_Builder with null record;

   function Defining_Package_Name return Defining_Package_Name_Type'Class;
   procedure Add_Identifier
     (To         : in out Defining_Package_Name_Type'Class;
      Identifier : in     String);

   type Package_Spec_Declaration_Type is
     new Program_Builder with null record;

   function Package_Spec
     (Defining_Package_Name    : in     Defining_Package_Name_Type'Class;
      Package_Spec_Declaration : in     Package_Spec_Declaration_Type'Class)
      return Package_Spec_Type'Class;

end Aquarius.Drys;
