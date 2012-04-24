with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Text_IO;

package body Aquarius.Projects.File_View is

   type File_View_Type is new Root_Project_View with null record;

   overriding
   procedure Reload (View : in out File_View_Type);

   type File_Tree_Type is new Root_Project_Tree with null record;

   function Create_Tree
     (Project : not null access Aquarius_Project_Type'Class)
     return Aquarius.Trees.Tree;

   function New_File_Tree_Node (Name : String)
                               return Aquarius.Trees.Tree;

   function Get_Relative_Path (Start    : String;
                               Path     : String)
                              return String;

   ----------------------
   -- Create_File_View --
   ----------------------

   procedure Create_File_View
     (Project : access Aquarius_Project_Type'Class)
   is
      Result : File_View_Type;
   begin
      Result.Name    := Aquarius.Names.To_Aquarius_Name ("File View");
      Result.Project := Aquarius_Project (Project);
      Result.View_Contents := Create_Tree (Project);
      Project.Views.Append (new File_View_Type'(Result));
   end Create_File_View;

   -----------------
   -- Create_Tree --
   -----------------

   function Create_Tree
     (Project : not null access Aquarius_Project_Type'Class)
     return Aquarius.Trees.Tree
   is
      use Aquarius.Trees;
      use Aquarius.Source.File_System;
      Root : constant Tree := New_File_Tree_Node ("Files");
      Project_Path  : constant String :=
        Aquarius.Names.To_String (Project.Full_Path);
   begin
      for I in 1 .. Search_Path_Count (Project.Search_Path) loop
         declare
            use Ada.Directories;
            Full_Path     : constant String :=
              Get_Search_Path (Project.Search_Path, I);
            Relative_Path : constant String :=
              Get_Relative_Path (Project_Path, Full_Path);
            New_Folder    : constant Tree :=
              New_File_Tree_Node (Relative_Path);
            New_File      : Tree;
            Search        : Search_Type;
            Next_Entry    : Directory_Entry_Type;
         begin
            if Exists (Full_Path) then
               Root.Add_Child (New_Folder);
               Start_Search (Search, Full_Path, "",
                             (Ordinary_File => True, others => False));
               while More_Entries (Search) loop
                  Get_Next_Entry (Search, Next_Entry);
                  if Project.Have_Buffer (Simple_Name (Next_Entry)) then
                     New_File := New_File_Tree_Node (Simple_Name (Next_Entry));
                     New_Folder.Add_Child (New_File);
                  end if;
               end loop;
               End_Search (Search);
            else
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     "no such directory: " &
                                       Full_Path);
            end if;
         exception
            when Ada.IO_Exceptions.Name_Error =>
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     "bad path: " &
                                       Full_Path);
         end;
      end loop;
      return Root;
   end Create_Tree;

   -----------------------
   -- Get_Relative_Path --
   -----------------------

   function Get_Relative_Path (Start    : String;
                               Path     : String)
                              return String
   is
   begin

      if Path'Length >= Start'Length and then
        Path (Path'First .. Path'First + Start'Length - 1) = Start
      then
         declare
            Result : constant String :=
              Path (Start'Length + Path'First .. Path'Last);
            Index  : Positive := Result'First;
         begin
            while Index <= Result'Last and then
              (Result (Index) = '/' or else Result (Index) = '.')
            loop
               Index := Index + 1;
            end loop;
            return Result (Index .. Result'Last);
         end;
      else
         return Path;
      end if;
   end Get_Relative_Path;

   ------------------------
   -- New_File_Tree_Node --
   ------------------------

   function New_File_Tree_Node (Name : String)
                               return Aquarius.Trees.Tree
   is
      use Ada.Strings.Unbounded;
      Result : constant Aquarius.Trees.Tree :=
        new File_Tree_Type'(Aquarius.Trees.Root_Tree_Type with
                              Label => To_Unbounded_String (Name),
                              Target => null);
   begin
      return Result;
   end New_File_Tree_Node;

   ------------
   -- Reload --
   ------------

   overriding
   procedure Reload (View : in out File_View_Type) is
   begin
      View.View_Contents := Create_Tree (View.Project);
   end Reload;

end Aquarius.Projects.File_View;
