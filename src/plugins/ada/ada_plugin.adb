with Ada.Characters.Latin_1;

with Aquarius.Entries.Packages;
with Aquarius.Errors;
with Aquarius.VM;

with Ada_Plugin.App_A01;
with Ada_Plugin.Names;
with Ada_Plugin.Generated;

package body Ada_Plugin is

   Global_Ada_Plugin : Ada_Plugin_Access;

   procedure Create_Properties
     (Plugin  : not null access Ada_Plugin_Type'Class;
      Grammar : in out     Aquarius.Grammars.Aquarius_Grammar_Record'Class);

   procedure Create_Change_Handlers
     (Plugin  : not null access Ada_Plugin_Type'Class);

   procedure Create_Environment
     (Plugin     : not null access Ada_Plugin_Type'Class);

   function To_File_Name (Package_Name : String) return String;

   function Make_Full_Name (Parent : Aquarius.Entries.Table_Entry;
                            Child  : String)
                           return String;

   ----------------------------
   -- Create_Change_Handlers --
   ----------------------------

   procedure Create_Change_Handlers
     (Plugin  : not null access Ada_Plugin_Type'Class)
   is
   begin
      Ada_Plugin.Names.Create_Change_Handlers (Plugin);
   end Create_Change_Handlers;

   ------------------------
   -- Create_Environment --
   ------------------------

   procedure Create_Environment
     (Plugin     : not null access Ada_Plugin_Type'Class)
   is
      use Aquarius.VM;
      use Ada.Characters.Latin_1;
   begin
      Plugin.New_Command
        (Internal_Name  => "Create_Package",
         External_Name  => "Create Package",
         Menu_Path      => "Ada",
         Description    => "Create an empty package skeleton",
         Definition     =>
           Make_List
           ((Get_Value (Plugin.Environment, "parse"),
             Make_List
             ((Get_Value (Plugin.Environment, "create_tree"),
               To_Value ("compilation_unit"))),
             To_Value ("package Untitled is " &
                         (LF, LF) &
                         "end Untitled;"))
           )
        );

   end Create_Environment;

   -----------------------
   -- Create_Properties --
   -----------------------

   procedure Create_Properties
     (Plugin  : not null access Ada_Plugin_Type'Class;
      Grammar : in out     Aquarius.Grammars.Aquarius_Grammar_Record'Class)
   is
   begin
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Property_Left, "ada-left", False, True);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Property_Right, "ada-right", False, True);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Property_Function, "ada-function", False, False);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Property_Object_Reference,
         "ada-object-reference",
         False, False);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Property_Node, "ada-node", False, False);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Property_Expression,
         "ada-expression", False, True);

      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Object_Property, "object", True, False);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Package_Property, "package", True, False);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Record_Property, "record", True, False);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Procedure_Property, "procedure", True, False);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Package_Spec_Property, "spec", True, False);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Package_Body_Property, "body", True, False);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Defining_Instance_Property,
         "defining-instance",
         Inherited => True,
         Has_Value => False);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Defined_Name_Property,
         "ada-defined-name",
         Inherited => False,
         Has_Value => True);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Attribute_Property,
         "attribute",
         Inherited => True,
         Has_Value => False);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Last_Identifier_Property,
         "last_identifier",
         Inherited => True,
         Has_Value => True);

      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Type_Error_Tag, "type-error-tag",
         Inherited => False,
         Has_Value => False);

      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Object_Reference_Property,
         "ada-object-reference",
         Inherited => True,
         Has_Value => True);

      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Type_Declaration_Property,
         "ada-type-declaration",
         Inherited => True,
         Has_Value => True);

      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Compilation_Unit_Property,
         "ada-compilation-unit",
         Inherited => True,
         Has_Value => True);

      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Top_Label_Property,
         "ada-top-label",
         Inherited => True,
         Has_Value => True);

      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Out_Label_Property,
         "ada-out-label",
         Inherited => True,
         Has_Value => True);

      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Private_Section_Property,
         "ada-private-section",
         Inherited => True,
         Has_Value => False);

      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Has_Address_Property,
         "ada-node-has-address",
         Inherited => False,
         Has_Value => False);

      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Has_Scalar_Property,
         "ada-node-has-scalar",
         Inherited => False,
         Has_Value => False);

   end Create_Properties;

   ----------
   -- Load --
   ----------

   overriding
   procedure Load (Plugin  : not null access Ada_Plugin_Type;
                   Grammar : in     Aquarius.Grammars.Aquarius_Grammar)
   is
   begin

      Aquarius.Plugins.Load
        (Aquarius.Plugins.Aquarius_Plugin_Type (Plugin.all)'Access,
         Grammar);

      Plugin.Create_Properties (Grammar.all);
      Ada_Plugin.Generated.Bind_Actions (Plugin.all, Grammar);

      Plugin.Create_Environment;

      Plugin.Create_Change_Handlers;

      Ada_Plugin.App_A01.Load_Package_Standard (Plugin);
      Global_Ada_Plugin := Ada_Plugin_Access (Plugin);

   end Load;

   ------------------------
   -- Load_Child_Package --
   ------------------------

   function Load_Child_Package (Project  : Aquarius.Projects.Aquarius_Project;
                                Tree     : Aquarius.Programs.Program_Tree;
                                Parent   : Aquarius.Entries.Table_Entry;
                                Child    : String)
                               return Aquarius.Entries.Table_Entry
   is
      use Aquarius.Programs;
      Withed_Package : Program_Tree;
      Package_Name   : constant String := Make_Full_Name (Parent, Child);
   begin

      Withed_Package :=
        Project.Get_Program (To_File_Name (Package_Name));

      if Withed_Package = null then
         Aquarius.Errors.Error (Tree,
                                "file """ & To_File_Name (Package_Name) &
                                  """ not found");
         return null;
      end if;

      declare
         Ref : constant Aquarius.Entries.Table_Entry :=
           Aquarius.Entries.Packages.New_Package_Reference
                   (Tree, Withed_Package.Get_Entry);
      begin
         if Aquarius.Entries.Is_Null (Parent) then
            Tree.Symbol_Table.Insert (Ref);
         else
            Aquarius.Entries.Packages.Add_Child_Package (Parent, Ref);
         end if;

         return Ref;
      end;
   end Load_Child_Package;

   --------------------
   -- Make_Full_Name --
   --------------------

   function Make_Full_Name (Parent : Aquarius.Entries.Table_Entry;
                            Child  : String)
                           return String
   is
      use Aquarius.Entries;
   begin
      if Parent = null then
         return Child;
      else
         return Make_Full_Name (Parent.Entry_Owner, Parent.Name) &
           "." & Child;
      end if;
   end Make_Full_Name;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Plugin : Ada_Plugin_Type) return String is
      pragma Unreferenced (Plugin);
   begin
      return "Ada";
   end Name;

   ------------
   -- Plugin --
   ------------

   function Plugin return Ada_Plugin_Access is
   begin
      return Global_Ada_Plugin;
   end Plugin;

   ------------------
   -- To_File_Name --
   ------------------

   function To_File_Name (Package_Name : String) return String is
      Result : String := Package_Name & ".ads";
   begin
      for I in Package_Name'Range loop
         if Result (I) = '.' then
            Result (I) := '-';
         end if;
      end loop;
      return Result;
   end To_File_Name;

   -------------
   -- Version --
   -------------

   overriding
   function Version
     (Plugin : Ada_Plugin_Type)
     return String
   is
      pragma Unreferenced (Plugin);
   begin
      return "0.1";
   end Version;

end Ada_Plugin;
