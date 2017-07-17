with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Ada.Directories;

with Aquarius.Config_Paths;

package body Ack.Files is

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   Class_Path : String_Lists.List;

   function Base_File_Name
     (Parent : Entity_Id;
      Name   : Name_Id)
      return String;

   --------------------
   -- Base_File_Name --
   --------------------

   function Base_File_Name
     (Parent : Entity_Id;
      Name   : Name_Id)
      return String
   is
      File_Name : constant String :=
                    Ada.Characters.Handling.To_Lower (To_String (Name));
   begin
      if Parent = No_Entity then
         return File_Name;
      else
         return Base_File_Name (Get_Context (Parent), Get_Name (Parent))
           & "-" & File_Name;
      end if;
   end Base_File_Name;

   --------------------
   -- Base_File_Name --
   --------------------

   function Base_File_Name (Class : Entity_Id) return String is
   begin
      return Base_File_Name (Get_Context (Class), Get_Name (Class));
   end Base_File_Name;

   ---------------------
   -- Find_Class_File --
   ---------------------

   function Find_Class_File
     (Referrer : Aquarius.Programs.Program_Tree;
      Parent   : Entity_Id;
      Name     : Name_Id)
      return String
   is
      File_Name : constant String :=
                    Base_File_Name (Parent, Name) & ".aqua";
   begin
      if Class_Path.Is_Empty then
         Class_Path.Append (Ada.Directories.Current_Directory);
         Class_Path.Append (Aquarius.Config_Paths.Config_File
                            ("aqua"));
         Class_Path.Append (Aquarius.Config_Paths.Config_File
                            ("aqua/standard"));
         Class_Path.Append (Aquarius.Config_Paths.Config_File
                            ("aqua/generated"));
      end if;

      declare
         Local_Path : constant String := Referrer.Source_Directory;
      begin
         if Ada.Directories.Exists (Local_Path & "/" & File_Name) then
            return Local_Path & "/" & File_Name;
         end if;
      end;

      for Path of Class_Path loop
         if Ada.Directories.Exists (Path & "/" & File_Name) then
            return Path & "/" & File_Name;
         end if;
      end loop;

--        Ada.Text_IO.Put_Line
--          (Ada.Text_IO.Standard_Error,
--           "Cannot find class file " & File_Name);

      return "";

   end Find_Class_File;

end Ack.Files;
