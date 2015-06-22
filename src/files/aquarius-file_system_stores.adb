with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Aquarius.Actions;
with Aquarius.Grammars.Manager;
with Aquarius.Loader;

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
         return Store.Loaded_Programs (Name);
      else
         return null;
      end if;
   end Get_Program;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Store : not null access Root_File_System_Store)
   is

      procedure Recurse
        (Path : String);

      -------------
      -- Recurse --
      -------------

      procedure Recurse
        (Path : String)
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
                        begin
                           if Program /= null then
                              Store.Loaded_Programs.Insert
                                (Key      => Simple_Name (File_Name),
                                 New_Item => Program);
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
                  Recurse (File_Name);
               end if;
            end;
         end loop;

         End_Search (Search);
      end Recurse;

   begin
      Recurse (Aquarius.Names.To_String (Store.Base_Path));
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

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Komnenos.Session_Objects.Register_Session_Object
        (File_System_Store_Name, New_File_System_Store'Access);
   end Register;

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
      Item.Extensions.Scan (Add_Extension'Access);
      Config.Add (Ext_Config);
   end To_Config;

end Aquarius.File_System_Stores;
