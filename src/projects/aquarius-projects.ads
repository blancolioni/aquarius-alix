with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Aquarius.Buffers;
with Aquarius.Entries;
with Aquarius.Names;
with Aquarius.Programs;
with Aquarius.Source.File_System;
with Aquarius.Trees;
with Aquarius.UI;

package Aquarius.Projects is

   type Aquarius_Project_Type is
     new Root_Aquarius_Object
     and Aquarius.Programs.Root_Program_Tree_Store
   with private;

   overriding
   function Name (Project : Aquarius_Project_Type)
                 return String;

   procedure Add_Search_Path
     (Project : not null access Aquarius_Project_Type'Class;
      Path    : in     String);

   overriding
   function Get_Program (Project   : not null access Aquarius_Project_Type;
                         File_Name : String)
                        return Aquarius.Programs.Program_Tree;

   function Get_Buffer
     (Project     : not null access Aquarius_Project_Type'Class;
      File_Name   : String;
      Synchronous : Boolean)
      return Aquarius.Buffers.Aquarius_Buffer;
   --  Get_Buffer will return the buffer if it exists, otherwise it
   --  will create a new buffer and add it to the project
   --  If Synchronous is True, the semantic analysis will happen
   --  before the function returns, otherwise it will be performed
   --  in the background.

   procedure Load_Dependency
     (Project   : in out Aquarius_Project_Type'Class;
      Reference : Aquarius.Programs.Program_Tree;
      File_Name : String);

   function Have_Buffer (Project   : Aquarius_Project_Type'Class;
                         File_Name : String)
                        return Boolean;

   function Get_Main_Buffer
     (Project : not null access Aquarius_Project_Type'Class)
      return Aquarius.Buffers.Aquarius_Buffer;
   --  Creates the main buffer if it doesn't already exist

   procedure Add_Main (Project : not null access Aquarius_Project_Type'Class;
                       Name    : in     String);

   type Aquarius_Project is access all Aquarius_Project_Type'Class;

   function New_Project
     (Name      : String;
      Directory : String;
      UI        : not null access Aquarius.UI.Aquarius_UI'Class)
     return Aquarius_Project;

   function New_Project
     (Directory : String;
      UI        : not null access Aquarius.UI.Aquarius_UI'Class)
     return Aquarius_Project;

   function New_Empty_Project
     return Aquarius_Project;

   function New_Default_Project
     (For_File    : String;
      UI          : not null access Aquarius.UI.Aquarius_UI'Class)
      return Aquarius_Project;
   --  New_Default_Project: creates a project with default settings,
   --  which references only the given file.  No attempt is made to
   --  load the file, which is important because this project is used
   --  to load files which have no project themselves.

   type Root_Project_View is
     abstract new Root_Aquarius_Object with private;

   overriding
   function Name (View : Root_Project_View) return String;

   function Contents (View : not null access Root_Project_View)
                     return Aquarius.Trees.Tree;

   function View_Count (Project : not null access Aquarius_Project_Type'Class)
                       return Natural;

   function View (Project : not null access Aquarius_Project_Type'Class;
                  Index   : in     Positive)
                 return access Root_Project_View'Class;

   procedure Add_Entry (To_Project : in out Aquarius_Project_Type'Class;
                        New_Entry  : in     Aquarius.Entries.Table_Entry);

   procedure Remove_Entry
     (From_Project : in out  Aquarius_Project_Type'Class;
      Old_Entry    : in     Aquarius.Entries.Table_Entry);

   function Filter_Entries (Project : Aquarius_Project_Type'Class;
                            Text    : String;
                            Max     : Integer  := 100)
                           return Aquarius.Entries.Array_Of_Entries;

   type Root_Project_Tree is
     abstract new Aquarius.Trees.Root_Tree_Type with private;

   type Project_Tree is access all Root_Project_Tree'Class;

   function Target (Item : Root_Project_Tree'Class)
                   return Aquarius.Programs.Program_Tree;

   function Label (Item : Root_Project_Tree'Class)
                  return String;

   function New_Default_Project
     (File_Path : String)
     return Aquarius_Project;
   --  Create a default project which contains only the given file

private

   type Root_Project_View is
     abstract new Root_Aquarius_Object with
      record
         Name          : Aquarius.Names.Aquarius_Name;
         Project       : Aquarius_Project;
         View_Contents : Aquarius.Trees.Tree;
      end record;

   type Root_Project_Tree is
     abstract new Aquarius.Trees.Root_Tree_Type with
      record
         Label     : Ada.Strings.Unbounded.Unbounded_String;
         Target    : Aquarius.Programs.Program_Tree;
      end record;

   overriding
   function Name (Tree : Root_Project_Tree) return String;

   overriding
   function Text (Tree : Root_Project_Tree) return String;

   overriding
   function Image (Tree : Root_Project_Tree) return String;

   overriding
   function Keep_Siblings
     (Item : Root_Project_Tree)
     return Boolean;

   overriding
   function Keep_Parent
     (Item : Root_Project_Tree)
     return Boolean;

   package Loaded_Buffer_Vector is
      new Ada.Containers.Vectors (Positive,
                                  Aquarius.Buffers.Aquarius_Buffer,
                                  Aquarius.Buffers."=");

   type Project_View is access all Root_Project_View'Class;

   package View_Vector is
      new Ada.Containers.Vectors (Positive, Project_View);

   package Project_Entries_Vector is
      new Ada.Containers.Vectors (Positive,
                                  Aquarius.Entries.Table_Entry,
                                  Aquarius.Entries."=");

   type Aquarius_Project_Type is
     new Root_Aquarius_Object
     and Aquarius.Programs.Root_Program_Tree_Store
   with
      record
         Name          : Aquarius.Names.Aquarius_Name;
         Full_Path     : Aquarius.Names.Aquarius_Name;
         Search_Path   : Aquarius.Source.File_System.Search_Path_List;
         Main_Source   : Aquarius.Names.Aquarius_Name;
         Main_Buffer   : Aquarius.Buffers.Aquarius_Buffer;
         Buffers       : Loaded_Buffer_Vector.Vector;
         Views         : View_Vector.Vector;
         Grammar_Names : Aquarius.Names.Name_List;
         Project_UI    : access Aquarius.UI.Aquarius_UI'Class;
         Entries       : Project_Entries_Vector.Vector;
      end record;

end Aquarius.Projects;
