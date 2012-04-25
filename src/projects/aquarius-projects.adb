with Ada.Directories;
with Ada.Strings.Fixed;

with Aquarius.Configuration;
with Aquarius.Errors;
with Aquarius.Grammars.Manager;

with Aquarius.Projects.Entry_View;
with Aquarius.Projects.File_View;
with Aquarius.Projects.Package_View;

with Aquarius.UI.Console;

with Aquarius.Tasks;

package body Aquarius.Projects is

   ---------------
   -- Add_Entry --
   ---------------

   procedure Add_Entry
     (To_Project : in out Aquarius_Project_Type'Class;
      New_Entry  : in     Aquarius.Entries.Table_Entry)
   is
      use Project_Entries_Vector;
      It : Cursor := To_Project.Entries.First;
   begin
      while Has_Element (It) loop
         declare
            E : constant Aquarius.Entries.Table_Entry := Element (It);
         begin
            exit when E.Name > New_Entry.Name;
         end;
         Next (It);
      end loop;

      if not Has_Element (It) then
         To_Project.Entries.Append (New_Entry);
      else
         To_Project.Entries.Insert (It, New_Entry);
      end if;
      Aquarius.Tasks.Set_Changed ("GUI");
   end Add_Entry;

   --------------
   -- Add_Main --
   --------------

   procedure Add_Main (Project : not null access Aquarius_Project_Type'Class;
                       Name    : in     String)
   is
      use type Aquarius.Buffers.Aquarius_Buffer;
   begin
      Project.Main_Source := Aquarius.Names.To_Aquarius_Name (Name);
      Project.Main_Buffer := Project.Get_Buffer (Name, False);
      --  Main_Buffer may be null at this point, since it's not
      --  required to set up the source folders before the main
      --  file name.
   end Add_Main;

   ---------------------
   -- Add_Search_Path --
   ---------------------

   procedure Add_Search_Path
     (Project : not null access Aquarius_Project_Type'Class;
      Path    : in     String)
   is
   begin
      Aquarius.Source.File_System.Add_Path
        (Project.Search_Path,
         Path);
   end Add_Search_Path;

   --------------
   -- Contents --
   --------------

   function Contents (View : not null access Root_Project_View)
                     return Aquarius.Trees.Tree
   is
   begin
      return View.View_Contents;
   end Contents;

   --------------------
   -- Filter_Entries --
   --------------------

   function Filter_Entries (Project : Aquarius_Project_Type'Class;
                            Text    : String;
                            Max     : Integer  := 100)
                           return Aquarius.Entries.Array_Of_Entries
   is
      use Project_Entries_Vector;
      Result : Aquarius.Entries.Array_Of_Entries (1 .. Max);
      Count  : Natural := 0;
      It     : Cursor := Project.Entries.First;
   begin
      while Has_Element (It) and then Count < Max loop
         if Ada.Strings.Fixed.Index (Element (It).Name, Text) > 0 then
            Count := Count + 1;
            Result (Count) := Element (It);
         end if;
         Next (It);
      end loop;
      return Result (1 .. Count);
   end Filter_Entries;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
     (Project     : not null access Aquarius_Project_Type'Class;
      File_Name   : String;
      Synchronous : Boolean)
      return Aquarius.Buffers.Aquarius_Buffer
   is

      use Loaded_Buffer_Vector;
      It : Cursor := Project.Buffers.First;
   begin
      while Has_Element (It) loop
         if Element (It).File_Simple_Name = File_Name then
            return Element (It);
         end if;
         Next (It);
      end loop;

      declare
         Path : constant String :=
           Aquarius.Source.File_System.Find_File (Project.Search_Path,
                                                  File_Name);
         Buffer : Aquarius.Buffers.Aquarius_Buffer;
      begin
         if Path /= "" then
            declare
               G : constant Aquarius.Grammars.Aquarius_Grammar :=
                 Aquarius.Grammars.Manager.Get_Grammar_For_File (Path);
            begin
               if not Aquarius.Names.Contains (Project.Grammar_Names,
                                               G.Name)
               then
                  Aquarius.Names.Append (Project.Grammar_Names, G.Name);
                  Project.Add_Search_Path
                    (Ada.Directories.Compose
                       (Aquarius.Configuration.Get_Library_Path, G.Name));
               end if;
            end;
            Buffer :=
              Aquarius.Buffers.New_Buffer_From_File
                (Project.Project_UI,
                 Path, Project);
            Project.Buffers.Append (Buffer);
            Buffer.Load (Synchronous);
            Aquarius.Tasks.Set_Changed ("GUI");
            return Buffer;
         else
            return null;
         end if;
      end;
   end Get_Buffer;

   ---------------------
   -- Get_Main_Buffer --
   ---------------------

   function Get_Main_Buffer
     (Project : not null access Aquarius_Project_Type'Class)
      return Aquarius.Buffers.Aquarius_Buffer
   is
      use type Aquarius.Buffers.Aquarius_Buffer;
   begin
      if Project.Main_Buffer = null then
         Project.Main_Buffer :=
           Get_Buffer (Project,
                       Aquarius.Names.To_String (Project.Main_Source),
                       False);
         if Project.Main_Buffer = null then
            Aquarius.Errors.Error
              (null,
               Aquarius.Names.To_String (Project.Main_Source) &
                 ": not found");
         end if;
      end if;

      return Project.Main_Buffer;
   end Get_Main_Buffer;

   -----------------
   -- Get_Program --
   -----------------

   overriding
   function Get_Program (Project   : not null access Aquarius_Project_Type;
                         File_Name : String)
                        return Aquarius.Programs.Program_Tree
   is
      use type Aquarius.Buffers.Aquarius_Buffer;
      Buffer : constant Aquarius.Buffers.Aquarius_Buffer :=
        Project.Get_Buffer (File_Name, True);
   begin
      if Buffer /= null then
         return Buffer.Program;
      else
         return null;
      end if;
   end Get_Program;

   -----------------
   -- Have_Buffer --
   -----------------

   function Have_Buffer (Project   : Aquarius_Project_Type'Class;
                         File_Name : String)
                        return Boolean
   is
      use Loaded_Buffer_Vector;
      It : Cursor := Project.Buffers.First;
   begin
      while Has_Element (It) loop
         if Element (It).File_Simple_Name = File_Name then
            return True;
         end if;
         Next (It);
      end loop;
      return False;
   end Have_Buffer;

   -----------
   -- Image --
   -----------

   overriding
   function Image (Tree : Root_Project_Tree) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Tree.Label);
   end Image;

   -----------------
   -- Keep_Parent --
   -----------------

   overriding
   function Keep_Parent
     (Item : Root_Project_Tree)
     return Boolean is
      pragma Unreferenced (Item);
   begin
      return True;
   end Keep_Parent;

   -------------------
   -- Keep_Siblings --
   -------------------

   overriding
   function Keep_Siblings
     (Item : Root_Project_Tree)
     return Boolean
   is
     pragma Unreferenced (Item);
   begin
      return True;
   end Keep_Siblings;

   -----------
   -- Label --
   -----------

   function Label (Item : Root_Project_Tree'Class)
                  return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Label);
   end Label;

   ---------------------
   -- Load_Dependency --
   ---------------------

   procedure Load_Dependency
     (Project   : in out Aquarius_Project_Type'Class;
      Reference : in     Aquarius.Programs.Program_Tree;
      File_Name : in     String)
   is
      use Loaded_Buffer_Vector;
      It : Cursor := Project.Buffers.First;
   begin
      while Has_Element (It) loop
         if Element (It).File_Simple_Name = File_Name then
            return;
         end if;
         Next (It);
      end loop;

      declare
         Path : constant String :=
           Aquarius.Source.File_System.Find_File (Project.Search_Path,
                                                  File_Name);
         Buffer : Aquarius.Buffers.Aquarius_Buffer;
      begin
         if Path = "" then
            Aquarius.Errors.Error (Reference,
                                   "file """ & File_Name &
                                     """ not found");
            return;
         end if;

         declare
            G : constant Aquarius.Grammars.Aquarius_Grammar :=
              Aquarius.Grammars.Manager.Get_Grammar_For_File (Path);
         begin
            if not Aquarius.Names.Contains (Project.Grammar_Names,
                                            G.Name)
            then
               Aquarius.Names.Append (Project.Grammar_Names, G.Name);
               Project.Add_Search_Path
                 (Ada.Directories.Compose
                    (Aquarius.Configuration.Get_Library_Path, G.Name));
            end if;
         end;
         Buffer :=
           Aquarius.Buffers.New_Buffer_From_File (Project.Project_UI,
                                                  Path, Project'Access);
         Project.Buffers.Append (Buffer);
         Buffer.Load (True);
      end;
   end Load_Dependency;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Project : Aquarius_Project_Type)
                 return String
   is
   begin
      return Aquarius.Names.To_String (Project.Name);
   end Name;

   ----------
   -- Name --
   ----------

   overriding
   function Name (View : Root_Project_View) return String is
   begin
      return Aquarius.Names.To_String (View.Name);
   end Name;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Tree : Root_Project_Tree) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Tree.Label);
   end Name;

   -------------------------
   -- New_Default_Project --
   -------------------------

   function New_Default_Project
     (File_Path : String)
     return Aquarius_Project
   is
   begin
      return New_Default_Project (File_Path, Aquarius.UI.Console.Console_UI);
   end New_Default_Project;

   -------------------------
   -- New_Default_Project --
   -------------------------

   function New_Default_Project
     (For_File    : String;
      UI          : not null access Aquarius.UI.Aquarius_UI'Class)
      return Aquarius_Project
   is
      Result : constant Aquarius_Project :=
                 New_Project ("Default",
                              Ada.Directories.Containing_Directory (For_File),
                              UI);
   begin
      Result.Main_Buffer :=
        Aquarius.Buffers.New_Buffer_From_File (UI      => UI,
                                               Path    => For_File,
                                               Store   => Result);
      return Result;
   end New_Default_Project;

   -----------------------
   -- New_Empty_Project --
   -----------------------

   function New_Empty_Project
     return Aquarius_Project
   is
   begin
      return New_Project ("Default", ".",
                          Aquarius.UI.Console.Console_UI);
   end New_Empty_Project;

   -----------------
   -- New_Project --
   -----------------

   function New_Project
     (Name      : String;
      Directory : String;
      UI        : not null access Aquarius.UI.Aquarius_UI'Class)
     return Aquarius_Project
   is
      Result : Aquarius_Project_Type;
   begin
      Result.Name       := Aquarius.Names.To_Aquarius_Name (Name);
      Result.Full_Path  := Aquarius.Names.To_Aquarius_Name (Directory);
      Aquarius.Source.File_System.Set_Base_Path (Result.Search_Path,
                                                 Directory);
      Result.Project_UI := UI;
      return new Aquarius_Project_Type'(Result);
   end New_Project;

   -----------------
   -- New_Project --
   -----------------

   function New_Project
     (Directory : String;
      UI        : not null access Aquarius.UI.Aquarius_UI'Class)
      return Aquarius_Project
   is
   begin
      return New_Project (Ada.Directories.Base_Name (Directory),
                          Ada.Directories.Containing_Directory (Directory),
                          UI);
   end New_Project;

   ------------------
   -- Remove_Entry --
   ------------------

   procedure Remove_Entry
     (From_Project : in out Aquarius_Project_Type'Class;
      Old_Entry    : in     Aquarius.Entries.Table_Entry)
   is
      use Project_Entries_Vector;
      It : Cursor := From_Project.Entries.Find (Old_Entry);
   begin
      if not Has_Element (It) then
         raise Constraint_Error with "no such entry: " & Old_Entry.Name;
      end if;
      From_Project.Entries.Delete (It);
   end Remove_Entry;

   ------------
   -- Target --
   ------------

   function Target (Item : Root_Project_Tree'Class)
                   return Aquarius.Programs.Program_Tree
   is
   begin
      return Item.Target;
   end Target;

   ----------
   -- Text --
   ----------

   overriding
   function Text (Tree : Root_Project_Tree) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Tree.Label);
   end Text;

   ----------
   -- View --
   ----------

   function View (Project : not null access Aquarius_Project_Type'Class;
                  Index   : in     Positive)
                 return access Root_Project_View'Class
   is
   begin
      return Project.Views.Element (Index);
   end View;

   ----------------
   -- View_Count --
   ----------------

   function View_Count (Project : not null access Aquarius_Project_Type'Class)
                       return Natural
   is
   begin
      if Project.Views.Last_Index = 0 then
         Aquarius.Projects.File_View.Create_File_View (Project);
         Aquarius.Projects.Package_View.Create_Package_View (Project);
         Aquarius.Projects.Entry_View.Create_Entry_View (Project);
      end if;
      return Project.Views.Last_Index;
   end View_Count;

end Aquarius.Projects;
