private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Fixed.Hash_Case_Insensitive;
private with Ada.Strings.Fixed.Equal_Case_Insensitive;

with Tropos;

private with Aquarius.Names.Sets;
with Aquarius.Programs;

with Komnenos.Entities;
with Komnenos.Session_Objects;

package Aquarius.File_System_Stores is

   File_System_Store_Name : constant String := "file_system_store";

   type Root_File_System_Store is
     new Komnenos.Entities.Program_Store_Interface
     and Komnenos.Session_Objects.Session_Object_Interface
     and Aquarius.Programs.Root_Program_Tree_Store
   with private;

   overriding function Get_Program
     (Store  : not null access Root_File_System_Store;
      Name   : String)
      return Aquarius.Programs.Program_Tree;

   overriding procedure Load
     (Store : not null access Root_File_System_Store);

   overriding function Config_Name
     (Store  : Root_File_System_Store)
      return String
   is (File_System_Store_Name);

   overriding procedure To_Config
     (Item : Root_File_System_Store;
      Config : in out Tropos.Configuration);

   overriding procedure From_Config
     (Item : not null access Root_File_System_Store;
      Config : Tropos.Configuration);

   type File_System_Store is access all Root_File_System_Store'Class;

   procedure Register;

private

   package Program_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Aquarius.Programs.Program_Tree,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive,
        "="             => Aquarius.Programs."=");

   type Root_File_System_Store is
     new Komnenos.Entities.Program_Store_Interface
     and Komnenos.Session_Objects.Session_Object_Interface
     and Aquarius.Programs.Root_Program_Tree_Store with
      record
         Base_Path       : Aquarius.Names.Aquarius_Name;
         Extensions      : Aquarius.Names.Sets.Name_Set;
         Loaded_Programs : Program_Maps.Map;
      end record;

end Aquarius.File_System_Stores;
