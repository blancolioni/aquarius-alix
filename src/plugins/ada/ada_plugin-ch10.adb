with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Aquarius.Entries.Objects;
with Aquarius.Entries.Packages;
with Aquarius.Errors;
with Aquarius.Projects;
with Aquarius.Source;
with Aquarius.Trees.Properties;
with Aquarius.Types.Maps;
with Aquarius.Values;

package body Ada_Plugin.Ch10 is

   function To_Spec_Name (Package_Name : String) return String;
   function To_Body_Name (Package_Name : String) return String;
   function To_File_Name (Package_Name : String;
                          Extension    : String)
                         return String;

   --  Is_Library_Package: returns True if the name refers
   --  to a package from the standard library.  Currently
   --  returns True for "ada", "system" and "interfaces".
   --  Name: the top-level package name.

   function Is_Library_Package (Name : String) return Boolean;

   -----------------------------
   -- Compilation_Unit_Before --
   -----------------------------

   procedure Compilation_Unit_Before
     (Unit : Program_Tree)
   is
      Standard_Ref : constant Aquarius.Entries.Table_Entry :=
        Aquarius.Entries.Packages.New_Package_Reference
        (Unit, Plugin.Get_Standard_Entry ("standard"));
   begin

      Unit.Create_Symbol_Table;
      Unit.Symbol_Table.Insert (Standard_Ref);
      Unit.Set_Property (Plugin.Compilation_Unit_Property, Unit);
      Aquarius.Entries.Packages.Make_Visible (Standard_Ref);
      declare
         Name : constant String :=
           Aquarius.Source.Get_File_Name
           (Aquarius.Source.Get_Source_File (Unit.Get_Location));
      begin
         if Ada.Strings.Fixed.Index (Name, ".ads") /= 0 then
            Unit.Set_Property (Plugin.Package_Spec_Property);
         elsif Ada.Strings.Fixed.Index (Name, ".adb") /= 0 then
            Unit.Set_Property (Plugin.Package_Body_Property);
         end if;
      end;

   end Compilation_Unit_Before;

   ------------------------
   -- Is_Library_Package --
   ------------------------

   function Is_Library_Package (Name : String) return Boolean is
   begin
      return Name = "ada" or else Name = "system" or else Name = "interfaces";
   end Is_Library_Package;

   ----------------------------------------
   -- Procedure_Spec_After_Defining_Name --
   ----------------------------------------

   procedure Procedure_Spec_After_Defining_Name
     (Procedure_Spec, Procedure_Name  : Program_Tree)
   is
      use Aquarius.Entries;
      use Aquarius.Trees.Properties;
      use Ada.Strings.Unbounded;

      Reference     : constant Program_Tree :=
        Procedure_Name.Program_Child ("qualified_reference");
      Defining_Name : constant Aquarius.Programs.Program_Tree :=
        Program_Tree
        (Reference.Property (Plugin.Last_Identifier_Property));
      Procedure_Entry : Aquarius.Entries.Table_Entry;
      Parent_Entry  : Aquarius.Entries.Table_Entry;
      pragma Warnings (Off, Parent_Entry);
      Compilation_Unit : constant Program_Tree :=
        Program_Tree (Procedure_Spec.Property
                        (Plugin.Compilation_Unit_Property));
   begin
      if Reference.Has_Entry then
         Parent_Entry := Reference.Get_Entry;
      end if;

--        Procedure_Spec.Create_Symbol_Table;

      Procedure_Entry :=
        Aquarius.Entries.Objects.New_Object_Entry
        (Defining_Name.Standard_Text,
         Procedure_Spec,
         Aquarius.Types.Maps.New_Map_Type (Aquarius.Types.Maps.Get_Void_Type),
         Aquarius.Values.No_Value,
         True);
      Procedure_Spec.Set_Entry (Procedure_Entry);

      if not Compilation_Unit.Has_Entry then
         Compilation_Unit.Set_Entry (Procedure_Entry);
      end if;

      Compilation_Unit.Symbol_Table.Insert (Procedure_Entry);

      declare
         Project : constant Aquarius.Projects.Aquarius_Project :=
                     Aquarius.Trees.Properties.Get_Project
                       (Compilation_Unit.all);
      begin
         Project.Add_Entry (Procedure_Entry);
      end;

   end Procedure_Spec_After_Defining_Name;

   ------------------
   -- To_Body_Name --
   ------------------

   function To_Body_Name (Package_Name : String) return String is
   begin
      return To_File_Name (Package_Name, "adb");
   end To_Body_Name;

   ------------------
   -- To_File_Name --
   ------------------

   function To_File_Name (Package_Name : String;
                          Extension    : String)
                         return String
   is
      Result : String := Package_Name & '.' & Extension;
   begin
      for I in Package_Name'Range loop
         if Result (I) = '.' then
            Result (I) := '-';
         end if;
      end loop;
      return Result;
   end To_File_Name;

   ------------------
   -- To_Spec_Name --
   ------------------

   function To_Spec_Name (Package_Name : String) return String is
   begin
      return To_File_Name (Package_Name, "ads");
   end To_Spec_Name;

   -----------------------
   -- With_Clause_After --
   -----------------------

   procedure With_Clause_After
     (With_Clause : Program_Tree)
   is
      use Ada.Strings.Unbounded;
      use Aquarius.Trees.Properties;
      Unit_Names  : constant Array_Of_Program_Trees :=
        With_Clause.Direct_Children ("withed_unit_name");
      Is_Limited  : constant Boolean :=
        With_Clause.Program_Child ("limited") /= null;
      Package_Name    : Unbounded_String;
      Project      : constant Aquarius.Projects.Aquarius_Project :=
        Aquarius.Trees.Properties.Get_Project (With_Clause.all);
   begin

      if Is_Limited then
         return;
      end if;

      for Unit_Index in Unit_Names'Range loop
         declare
            Unit_Name : Program_Tree renames Unit_Names (Unit_Index);
            Ids : constant Array_Of_Program_Trees :=
              Unit_Name.Direct_Children (Skip_Separators => True);
            Parent : Aquarius.Entries.Table_Entry := null;
            Library_Package : constant Boolean :=
              Is_Library_Package (Ids (Ids'First).Standard_Text);
         begin

            for Identifier_Index in Ids'Range loop
               declare
                  Identifier : Program_Tree renames Ids (Identifier_Index);
                  Standard_Name  : constant String :=
                                     Identifier.Standard_Text;
                  Withed_Package : Program_Tree;
                  Ref            : Aquarius.Entries.Table_Entry;
               begin

                  if Identifier_Index = Ids'First then
                     Package_Name := To_Unbounded_String (Standard_Name);
                  else
                     Package_Name := Package_Name & '.' & Standard_Name;
                  end if;

                  if Aquarius.Entries.Is_Null (Parent) then
                     Ref := With_Clause.Symbol_Table.Retrieve (Standard_Name);
                  else
                     Ref :=
                       Aquarius.Entries.Packages.Get_Child_Package
                       (Parent, Standard_Name);
                  end if;

                  if Aquarius.Entries.Is_Null (Ref) then

                     Withed_Package :=
                       Project.Get_Program (To_Spec_Name (To_String
                                                            (Package_Name)));

                     if Withed_Package = null then
                        Aquarius.Errors.Error (Identifier,
                                               "file """ &
                                                 To_Spec_Name
                                                 (To_String
                                                    (Package_Name)) &
                                                 """ not found");
                        return;
                     end if;

                     if not Library_Package then
                        Project.Load_Dependency
                          (Identifier,
                           To_Body_Name (To_String (Package_Name)));
                     end if;

                     if Withed_Package.Has_Entry then
                        declare
                           Withed_Entry        : constant
                             Aquarius.Entries.Table_Entry :=
                               Withed_Package.Get_Entry;
                        begin
                           Ref :=
                             Aquarius.Entries.Packages.New_Package_Reference
                               (Unit_Name, Withed_Entry);
                        end;
                     else
                        Aquarius.Errors.Error (Identifier,
                                               "no compilation unit");
                        exit;
                     end if;

                     if Aquarius.Entries.Is_Null (Parent) then
                        With_Clause.Symbol_Table.Insert (Ref);
                     else
                        Aquarius.Entries.Packages.Add_Child_Package
                          (Parent, Ref);
                     end if;
                  end if;

                  Parent := Ref;
               end;
            end loop;
         end;
      end loop;
   end With_Clause_After;

end Ada_Plugin.Ch10;
