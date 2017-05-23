with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Aquarius.Actions;
with Aquarius.Grammars.Manager;
with Aquarius.Loader;
with Aquarius.Plugins.Manager;
with Aquarius.Programs.Arrangements;
with Aquarius.Rendering.Files;
with Aquarius.Trees.Properties;

package body Aquarius.File_System_Stores is

   function New_File_System_Store
     return access Komnenos.Session_Objects.Session_Object_Interface'Class;

   function Load_Program (Path : String)
                           return Aquarius.Programs.Program_Tree;

   -----------------
   -- From_Config --
   -----------------

   overriding procedure From_Config
     (Item : not null access Root_File_System_Store;
      Config : Tropos.Configuration)
   is
   begin
      Ada.Text_IO.Put_Line ("Loading: file system store");
      Item.Base_Path :=
        Aquarius.Names.To_Aquarius_Name (Config.Get ("base_path"));
      Ada.Text_IO.Put_Line ("  base path: " & Config.Get ("base_path"));
      if Config.Contains ("folders") then
         Ada.Text_IO.Put_Line ("Folders:");
         for Folder_Config of Config.Child ("folders") loop
            Ada.Text_IO.Put ("  " & Folder_Config.Value);
            Item.Folders.Append (Folder_Config.Config_Name);
         end loop;
      end if;

      Ada.Text_IO.Put ("  extensions:");
      for Ext_Config of Config.Child ("extensions") loop
         Item.Extensions.Insert (Ext_Config.Config_Name);
         Ada.Text_IO.Put (" " & Ext_Config.Config_Name);
      end loop;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("done");
   end From_Config;

   -----------------
   -- Get_Program --
   -----------------

   overriding function Get_Program
     (Store  : not null access Root_File_System_Store;
      Name   : String)
      return Aquarius.Programs.Program_Tree
   is
   begin
      if Store.Loaded_Programs.Contains (Name) then
         return Store.Loaded_Programs (Name).Root;
      else
         return null;
      end if;
   end Get_Program;

   --------------------------
   -- Get_Program_Position --
   --------------------------

   function Get_Program_Position
     (Store : Root_File_System_Store'Class;
      Root  : Aquarius.Programs.Program_Tree)
      return Program_Maps.Cursor
   is
      use type Aquarius.Programs.Program_Tree;
   begin
      for Position in Store.Loaded_Programs.Iterate loop
         if Program_Maps.Element (Position).Root = Root then
            return Position;
         end if;
      end loop;
      return Program_Maps.No_Element;
   end Get_Program_Position;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Store : not null access Root_File_System_Store)
   is

      procedure Scan_Folder
        (Path    : String;
         Recurse : Boolean);

      -------------
      -- Recurse --
      -------------

      procedure Scan_Folder
        (Path    : String;
         Recurse : Boolean)
      is
         use Ada.Directories;
         use Ada.Strings.Fixed;
         Search           : Search_Type;
         Next             : Directory_Entry_Type;
      begin
         Start_Search
           (Search    => Search,
            Directory => Path,
            Pattern   => "",
            Filter    => (Ordinary_File => True,
                          Directory     => True,
                          others        => False));

         while More_Entries (Search) loop
            Get_Next_Entry (Search, Next);

            declare
               File_Name : constant String := Full_Name (Next);
            begin
               if File_Name (File_Name'Last) = '.' then
                  null;
               elsif Head (Base_Name (File_Name), 1) = "_"
                 or else Head (Base_Name (File_Name), 1) = "."
               then
                  null;
               elsif Kind (Next) = Ordinary_File then
                  if Store.Extensions.Contains
                    (Extension (File_Name))
                    and then not Store.Loaded_Programs.Contains
                      (Simple_Name (File_Name))
                  then
                     begin
                        declare
                           use Aquarius.Programs;
                           Program : constant Program_Tree :=
                                       Load_Program (File_Name);
                           New_Item : Program_Info;
                        begin
                           if Program /= null then
                              New_Item.Root := Program;
                              New_Item.Path :=
                                Ada.Strings.Unbounded.To_Unbounded_String
                                  (File_Name);
                              Store.Loaded_Programs.Insert
                                (Simple_Name (File_Name), New_Item);
                           end if;
                        end;
                     exception
                        when E : others =>
                           Ada.Text_IO.Put_Line
                             (Ada.Text_IO.Standard_Error,
                              "cannot load "
                              & Simple_Name (File_Name)
                              & ": "
                              & Ada.Exceptions.Exception_Message (E));
                     end;
                  end if;
               else
                  if Recurse then
                     Scan_Folder (File_Name, Recurse);
                  end if;
               end if;
            end;
         end loop;

         End_Search (Search);
      end Scan_Folder;

   begin
      if Store.Folders.Is_Empty then
         Scan_Folder (Aquarius.Names.To_String (Store.Base_Path), True);
      else
         for Folder of Store.Folders loop
            declare
               Full_Path : constant String :=
                             Aquarius.Names.To_String (Store.Base_Path)
                             & "/"
                             & Folder;
            begin
               Scan_Folder (Full_Path, False);
            end;
         end loop;
      end if;

      for Info of Store.Loaded_Programs loop
         declare
            Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                        Aquarius.Trees.Properties.Get_Grammar
                          (Info.Root);
         begin
            Grammar.Run_Action_Trigger
              (Info.Root, Aquarius.Actions.Project_Trigger);
         end;
      end loop;

      Aquarius.Plugins.Manager.Loaded_Plugin_Report;

   end Load;

   ------------------
   -- Load_Program --
   ------------------

   function Load_Program (Path : String)
                          return Aquarius.Programs.Program_Tree
   is
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                  Aquarius.Grammars.Manager.Get_Grammar_For_File
                    (Path);
      Program : constant Aquarius.Programs.Program_Tree :=
                  Aquarius.Loader.Load_From_File
                    (Grammar, Path);
   begin
      Grammar.Run_Action_Trigger (Program, Aquarius.Actions.Semantic_Trigger);
      return Program;
   end Load_Program;

   ---------------------------
   -- New_File_System_Store --
   ---------------------------

   function New_File_System_Store
     return access Komnenos.Session_Objects.Session_Object_Interface'Class
   is
      Result :  constant File_System_Store := new Root_File_System_Store;
   begin
      return Result;
   end New_File_System_Store;

   -------------
   -- On_Edit --
   -------------

   overriding procedure On_Edit
     (Store : in out Root_File_System_Store;
      Item  : not null access Komnenos.Source.Source_Tree_Interface'Class)
   is
      Program  : constant Aquarius.Programs.Program_Tree :=
                   Aquarius.Programs.Program_Tree (Item);
      Position : constant Program_Maps.Cursor :=
                   Store.Get_Program_Position (Program);
      Info     : Program_Info := Program_Maps.Element (Position);
   begin
      if Info.Clean then
         declare
            File_Path : constant String :=
                          Ada.Strings.Unbounded.To_String
                            (Info.Path);
            Backup_Path : constant String :=
                            File_Path & ".aq~";
         begin
            if Ada.Directories.Exists (Backup_Path) then
               Ada.Directories.Delete_File (Backup_Path);
            end if;
            Ada.Directories.Copy_File (File_Path, Backup_Path);
         end;
         Info.Clean := False;
      end if;

      Info.Changed := True;

      Store.Loaded_Programs.Replace_Element
        (Position, Info);

   end On_Edit;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Komnenos.Session_Objects.Register_Session_Object
        (File_System_Store_Name, New_File_System_Store'Access);
   end Register;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Store : not null access Root_File_System_Store)
   is
   begin
      for Position in Store.Loaded_Programs.Iterate loop
         declare
            Info : Program_Info :=
                     Program_Maps.Element (Position);
         begin
            if Info.Changed then
               declare
                  Renderer : Aquarius.Rendering.Aquarius_Renderer :=
                               Aquarius.Rendering.Files.File_Renderer
                                 (Ada.Strings.Unbounded.To_String
                                    (Info.Path));
               begin
                  Aquarius.Programs.Arrangements.Arrange
                    (Info.Root, 72);
                  Aquarius.Programs.Arrangements.Render
                    (Info.Root, Renderer);
               end;
               Info.Changed := False;
               Store.Loaded_Programs.Replace_Element (Position, Info);
            end if;
         end;
      end loop;
   end Save;

   ---------------
   -- To_Config --
   ---------------

   overriding procedure To_Config
     (Item : Root_File_System_Store;
      Config : in out Tropos.Configuration)
   is
      Ext_Config   : Tropos.Configuration :=
                       Tropos.New_Config ("extensions");

      procedure Add_Extension (Ext : Aquarius.Names.Aquarius_Name);

      -------------------
      -- Add_Extension --
      -------------------

      procedure Add_Extension (Ext : Aquarius.Names.Aquarius_Name) is
      begin
         Ext_Config.Add (Tropos.New_Config (Aquarius.Names.To_String (Ext)));
      end Add_Extension;

   begin
      Config.Add ("base_path",
                  Aquarius.Names.To_String (Item.Base_Path));
      if not Item.Folders.Is_Empty then
         declare
            Folders : Tropos.Configuration := Tropos.New_Config ("folders");
         begin
            for Folder of Item.Folders loop
               Folders.Add ("folder", Folder);
            end loop;
            Config.Add (Folders);
         end;
      end if;

      Item.Extensions.Scan (Add_Extension'Access);
      Config.Add (Ext_Config);
   end To_Config;

end Aquarius.File_System_Stores;
