private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Fixed.Hash_Case_Insensitive;
private with Ada.Strings.Fixed.Equal_Case_Insensitive;
private with Ada.Strings.Unbounded;

with Tropos;

private with Aquarius.Names.Sets;
with Aquarius.Programs;

with Komnenos.Entities;
with Komnenos.Source;

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

   overriding procedure On_Edit
     (Store : in out Root_File_System_Store;
      Item  : not null access Komnenos.Source.Source_Tree_Interface'Class);

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

--     procedure Create
--       (Item      : in out Root_File_System_Store'Class;
--        Base_Path : String);
--
--     procedure Add_Extension
--       (Item : in out Root_File_System_Store'Class;
--        Extension : String);
--
--     procedure Add_Folder
--       (Item        : in out Root_File_System_Store'Class;
--        Folder_Path : String);
--
   type File_System_Store is access all Root_File_System_Store'Class;

   procedure Register;

private

   type Program_Info is
      record
         Root         : Aquarius.Programs.Program_Tree;
         Path         : Ada.Strings.Unbounded.Unbounded_String;
         Clean        : Boolean := True;
         Changed      : Boolean := False;
      end record;

   package Program_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Program_Info,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive, String);

   type Root_File_System_Store is
     new Komnenos.Entities.Program_Store_Interface
     and Komnenos.Session_Objects.Session_Object_Interface
     and Aquarius.Programs.Root_Program_Tree_Store with
      record
         Base_Path       : Aquarius.Names.Aquarius_Name;
         Folders         : String_Vectors.Vector;
         Extensions      : Aquarius.Names.Sets.Name_Set;
         Loaded_Programs : Program_Maps.Map;
      end record;

   overriding procedure Save
     (Store : not null access Root_File_System_Store);

   function Get_Program_Position
     (Store : Root_File_System_Store'Class;
      Root  : Aquarius.Programs.Program_Tree)
     return Program_Maps.Cursor;

end Aquarius.File_System_Stores;
