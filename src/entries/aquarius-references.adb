with Ada.Strings.Fixed;

package body Aquarius.References is

   ------------------------
   -- Add_Implementation --
   ------------------------

   procedure Add_Implementation
     (List          : Reference_List;
      Name          : String;
      Standard_Name : String;
      Key           : String;
      Program       : Aquarius.Programs.Program_Tree)
   is
   begin
      Add_Specification (List, Name, Standard_Name,
                         "implementation-" & Key, Program);
   end Add_Implementation;

   -------------------
   -- Add_Reference --
   -------------------

   procedure Add_Reference
     (List      : Reference_List;
      Name      : String;
      Key       : String;
      Program       : Aquarius.Programs.Program_Tree)
   is
      pragma Unreferenced (List);
      pragma Unreferenced (Name);
      pragma Unreferenced (Key);
      pragma Unreferenced (Program);
   begin
      null;
   end Add_Reference;

   -----------------------
   -- Add_Specification --
   -----------------------

   procedure Add_Specification
     (List          : Reference_List;
      Name          : String;
      Standard_Name : String;
      Key           : String;
      Program       : Aquarius.Programs.Program_Tree)
   is
      use Sorted_List_Of_References;
      It : Cursor := List.Sorted_List.First;
      U_Name : constant Unbounded_String :=
                 To_Unbounded_String (Name);
      U_Std  : constant Unbounded_String :=
                 To_Unbounded_String (Standard_Name);
      U_Key  : constant Unbounded_String :=
                 To_Unbounded_String (Key);
   begin
      while Has_Element (It) loop
         if Element (It).Standard_Name > Standard_Name then
            List.Sorted_List.Insert (It, (U_Name, U_Std, U_Key, Program));
            return;
         end if;
         Next (It);
      end loop;

      List.Sorted_List.Append ((U_Name, U_Std, U_Key, Program));

   end Add_Specification;

   -----------------------
   -- Clear_Source_File --
   -----------------------

   procedure Clear_Source_File
     (List : Reference_List;
      File : String)
   is
      use Sorted_List_Of_References;
      It : Cursor := List.Sorted_List.First;
   begin
      while Has_Element (It) loop
         declare
            Next_It    : constant Cursor := Next (It);
            Program    : constant Aquarius.Programs.Program_Tree :=
                           Element (It).Program;
            File_Name  : constant String := Program.Location_Name;
         begin
            if File_Name = File then
               List.Sorted_List.Delete (It);
            end if;
            It := Next_It;
         end;
      end loop;
   end Clear_Source_File;

   ------------
   -- Filter --
   ------------

   function Filter
     (List : Reference_List;
      Text : String;
      Max  : Positive)
      return Array_Of_Locations
   is
      Result : Array_Of_Locations (1 .. Max);
      Count  : Natural := 0;
   begin
      for It in List.Sorted_List.Iterate loop
         declare
            use Sorted_List_Of_References;
         begin
            if Text = ""
              or else
                Ada.Strings.Fixed.Index
                  (To_String (Element (It).Standard_Name), Text) > 0
            then
               Count := Count + 1;
               Result (Count) := Reference_Cursor (It);
               exit when Count = Max;
            end if;
         end;
      end loop;
      return Result (1 .. Count);
   end Filter;

   ----------
   -- Find --
   ----------

   function Find
     (List : Reference_List;
      Name : String)
      return Array_Of_Locations
   is
      use Sorted_List_Of_References;
      Max    : constant := 100;
      Result : Array_Of_Locations (1 .. Max);
      Count  : Natural := 0;
   begin
      for It in List.Sorted_List.Iterate loop
         if Element (It).Standard_Name = Name then
            Count := Count + 1;
            Result (Count) := Reference_Cursor (It);
            exit when Count = Max;
         end if;
      end loop;
      return Result (1 .. Count);
   end Find;

   ---------------------
   -- Find_References --
   ---------------------

   function Find_References
     (List  : Reference_List;
      Name  : String;
      Key   : String)
      return Array_Of_Locations
   is
      pragma Unreferenced (List);
      pragma Unreferenced (Name);
      pragma Unreferenced (Key);
      Result : Array_Of_Locations (1 .. 0);
   begin
      return Result;
   end Find_References;

   ------------------------
   -- New_Reference_List --
   ------------------------

   function New_Reference_List
      return Reference_List
   is
   begin
      return new Reference_List_Record;
   end New_Reference_List;

   --------------------
   -- Reference_Name --
   --------------------

   function Reference_Name (Position : Reference_Cursor) return String is
      use Sorted_List_Of_References;
   begin
      return To_String (Element (Cursor (Position)).Name);
   end Reference_Name;

   ------------------------
   -- Reference_Location --
   ------------------------

   function Reference_Program (Position : Reference_Cursor)
                               return Aquarius.Programs.Program_Tree
   is
      use Sorted_List_Of_References;
   begin
      return Element (Cursor (Position)).Program;
   end Reference_Program;

end Aquarius.References;
