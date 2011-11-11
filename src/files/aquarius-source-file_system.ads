private with Ada.Text_IO;
private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

private with Aquarius.Paths;

package Aquarius.Source.File_System is

   type Search_Path_List is private;

   function Current_Directory return Search_Path_List;
   procedure Add_Path (List : in out Search_Path_List;
                       Path : in     String);

   procedure Set_Base_Path (List : in out Search_Path_List;
                            Path : in     String);

--     procedure Add_Path (List  : in out Search_Path_List;
--                         Start : in     String;
--                         Path  : in     String);

   function Read_File (File_Path   : in     String;
                       Search_Path : in     Search_Path_List;
                       Extension   : in     String            := "")
                      return Source_File;

   function Find_File (Search_Path : in Search_Path_List;
                       File_Name   : in String)
                      return String;

   function Read_File (File_Path   : in     String)
                      return Source_File;

   function Search_Path_Count (Search_Path : Search_Path_List)
                              return Natural;
   function Get_Search_Path (Search_Path : Search_Path_List;
                             Index       : Positive)
                            return String;

private

   type File_System_File is limited new Source_File_Record with
      record
         File_Name : Ada.Strings.Unbounded.Unbounded_String;
         Full_Path : Ada.Strings.Unbounded.Unbounded_String;
         File      : Ada.Text_IO.File_Type;
      end record;

   overriding
   function Get_File_Name (File : access File_System_File) return String;

   overriding
   function Get_Full_Path (File : access File_System_File) return String;

   overriding
   procedure Next_Line (File : access File_System_File);

   overriding
   function End_Of_File (File : access File_System_File) return Boolean;

   overriding
   procedure Close (Item : access File_System_File);

   package Path_Vector is
     new Ada.Containers.Vectors (Positive,
                                 Aquarius.Paths.Aquarius_Path,
                                 Aquarius.Paths."=");

   type Search_Path_List is
      record
         Base_Path : Aquarius.Paths.Aquarius_Path;
         Paths     : Path_Vector.Vector;
      end record;

end Aquarius.Source.File_System;
