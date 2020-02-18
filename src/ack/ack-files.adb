with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Ada.Directories;

with Komnenos.Entities.Tables;

with Aquarius.Config_Paths;
with Aquarius.File_System_Stores;

package body Ack.Files is

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   Class_Path  : String_Lists.List;
   Class_Store : Aquarius.File_System_Stores.File_System_Store;

   ---------------------
   -- Find_Class_File --
   ---------------------

   function Find_Class_File
     (Referrer : Aquarius.Programs.Program_Tree;
      Parent   : not null access constant Root_Entity_Type'Class;
      Name     : Name_Id)
      return String
   is
      use type Aquarius.File_System_Stores.File_System_Store;
      use type Aquarius.Programs.Program_Tree;

      File_Name : constant String :=
                    Parent.Base_Child_File_Name (Name) & ".aqua";
   begin
      if Class_Store = null then
         Class_Store :=
           new Aquarius.File_System_Stores.Root_File_System_Store;
         Class_Store.Create
           ("aqua", Aquarius.Config_Paths.Config_File ("aqua"));
         Class_Store.Add_Folder (".");
         Class_Store.Add_Folder ("standard");
         Class_Store.Add_Folder ("generated");
         Class_Store.Add_Extension ("aqua");

         declare
            Table : constant Komnenos.Entities.Entity_Table_Access :=
                      Komnenos.Entities.New_Table
                        ("aqua", Class_Store);
         begin
            Komnenos.Entities.Tables.Set_Table
              ("aqua", Table);
         end;

         Class_Path.Append (Ada.Directories.Current_Directory);
         Class_Path.Append (Aquarius.Config_Paths.Config_File
                            ("aqua"));
         Class_Path.Append (Aquarius.Config_Paths.Config_File
                            ("aqua/standard"));
         Class_Path.Append (Aquarius.Config_Paths.Config_File
                            ("aqua/generated"));
      end if;

      if Referrer /= null then
         declare
            Local_Path : constant String := Referrer.Source_Directory;
         begin
            if Ada.Directories.Exists (Local_Path & "/" & File_Name) then
               return Local_Path & "/" & File_Name;
            end if;
         end;
      end if;

      for Path of Class_Path loop
         if Ada.Directories.Exists (Path & "/" & File_Name) then
            return Path & "/" & File_Name;
         end if;
      end loop;

      return "";

   end Find_Class_File;

end Ack.Files;
