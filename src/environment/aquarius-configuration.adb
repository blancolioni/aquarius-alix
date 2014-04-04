with Ada.Containers.Vectors;
with Ada.Directories;
--  with Ada.Environment_Variables;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Aquarius.Grammars;
with Aquarius.Grammars.Manager;
with Aquarius.Loader;
with Aquarius.Messages.Console;
with Aquarius.Programs;
with Aquarius.Trees;

with Aquarius.Projects;
with Aquarius.Interaction.Console;
with Aquarius.UI.Console;

with Aquarius.Config_Paths;

package body Aquarius.Configuration is

   function Config_Folder_Path return String;

   function Aquarius_Config_File return String;

   package Config_Item_Vector is
      new Ada.Containers.Vectors (Positive, Cursor);

   type Config_Item_Record is new Root_Aquarius_Object with
      record
         Config_Type : Config_Item_Type;
         Parent      : Cursor;
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Children    : Config_Item_Vector.Vector;
         Value       : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding
   function Name (Item : Config_Item_Record) return String;

   Config : Cursor;

   procedure Build_Config (Top       : Cursor;
                           From_Tree : Aquarius.Trees.Tree);

   procedure Write_Config;

   procedure Path_Element_Not_Found (Element : String);

   function Follow_Path (Start : Cursor;
                         Path  : String)
                        return Cursor;

   --------------------------
   -- Aquarius_Config_File --
   --------------------------

   function Aquarius_Config_File return String is
   begin
      return Ada.Directories.Compose (Config_Folder_Path,
                                      "aquarius.config");
   end Aquarius_Config_File;

   ------------------
   -- Build_Config --
   ------------------

   procedure Build_Config (Top       : Cursor;
                           From_Tree : Aquarius.Trees.Tree)
   is
      use Ada.Strings.Unbounded;
      Items : constant Aquarius.Trees.Array_Of_Trees :=
        From_Tree.Get_Matching_Children ("config_item");
   begin
      for I in Items'Range loop
         declare
            Name   : constant String := Items (I).Leaf ("identifier").Text;
            Nested : constant Aquarius.Trees.Tree :=
              Items (I).Breadth_First_Search ("nested_item");
            Child  : constant Cursor := new Config_Item_Record;
         begin
            Top.Children.Append (Child);
            Child.Parent := Top;
            Child.Name   := To_Unbounded_String (Name);

            if Aquarius.Trees.Is_Null (Nested) then
               declare
                  Value : constant Aquarius.Trees.Tree :=
                    Items (I).Leaf ("value");
               begin
                  Child.Config_Type := Value_Item;
                  Child.Value       := To_Unbounded_String (Value.Text);
               end;
            else
               Child.Config_Type := Nested_Item;
               Build_Config (Child,
                             Items (I).Breadth_First_Search ("config_list"));
            end if;
         end;
      end loop;
   end Build_Config;

   -----------------
   -- Child_Count --
   -----------------

   function Child_Count (Position : Cursor) return Natural is
   begin
      case Position.Config_Type is
         when Value_Item =>
            return 0;
         when Nested_Item =>
            return Position.Children.Last_Index;
      end case;
   end Child_Count;

   ------------------------
   -- Config_Folder_Path --
   ------------------------

   function Config_Folder_Path return String is
   begin
      return Aquarius.Config_Paths.Config_Path;

      --  if Ada.Directories.Exists (".dev") then
      --     return Ada.Directories.Compose (Ada.Directories.Current_Directory,
      --                                     "config");
      --  else
      --     return Ada.Directories.Compose
      --       (Ada.Environment_Variables.Value ("HOME"), ".aquarius");
      --  end if;
   end Config_Folder_Path;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String) is
   begin
      Ada.Text_IO.Put_Line ("configuration error: " & Message);
      raise Configuration_Error with Message;
   end Error;

   ----------------
   -- Find_Child --
   ----------------

   function Find_Child (Start       : Cursor;
                        Child_Name  : String)
                       return Cursor
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Start.Config_Type = Value_Item then
         return null;
      end if;

      for I in 1 .. Start.Children.Last_Index loop
         if Start.Children.Element (I).Name = Child_Name then
            return Start.Children.Element (I);
         end if;
      end loop;
      return null;
   end Find_Child;

   -----------------
   -- Follow_Path --
   -----------------

   function Follow_Path (Start : Cursor;
                         Path  : String)
                        return Cursor
   is
      use Ada.Strings.Fixed;
      Result  : Cursor := Start;
      Current : Natural := 1;
      Next    : Natural;
   begin

      loop
         while Index (Path, "/", Current) = Current loop
            Current := Current + 1;
         end loop;

         Next := Index (Path, "/", Current);
         if Next = 0 then
            if Current < Path'Last then
               Next := Path'Last + 1;
            else
               return Result;
            end if;
         end if;

         Result := Find_Child (Result, Path (Current .. Next - 1));
         if Result = null then
            Path_Element_Not_Found (Path (Current .. Next - 1));
         end if;

         if Next > Path'Length then
            return Result;
         end if;

         Current := Next;
      end loop;

   end Follow_Path;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child   (Position : Cursor;
                         Index    : Positive)
                        return Cursor
   is
   begin
      if Position.Config_Type = Value_Item then
         return null;
      else
         return Position.Children.Element (Index);
      end if;
   end Get_Child;

   ----------------
   -- Get_Cursor --
   ----------------

   function Get_Cursor (Path : String) return Cursor is
   begin
      return Follow_Path (Config, Path);
   end Get_Cursor;

   ----------------------
   -- Get_Grammar_Path --
   ----------------------

   function Get_Grammar_Path (Grammar_Name : String) return String is
   begin
      return Config_Folder_Path & "/grammar/" & Grammar_Name & "/";
   end Get_Grammar_Path;

   ----------------------
   -- Get_Library_Path --
   ----------------------

   function Get_Library_Path return String is
   begin
      return Config_Folder_Path;
   end Get_Library_Path;

   --------------
   -- Get_Name --
   --------------

   function Get_Name    (Position : Cursor) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Position.Name);
   end Get_Name;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Position      : Cursor;
                       Value_Name    : String;
                       Default_Value : String := "")
                      return String
   is
      Child : constant Cursor := Find_Child (Position, Value_Name);
   begin
      if Child /= null then
         declare
            Result : constant String :=
                       Ada.Strings.Unbounded.To_String (Child.Value);
         begin
            if Result'Length >= 2
              and then Result (Result'First) = '"'
              and then Result (Result'Last) = '"'
            then
               return Result (Result'First + 1 .. Result'Last - 1);
            else
               return Result;
            end if;
         end;
      else
         return Default_Value;
      end if;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Position      : Cursor;
                       Value_Name    : String;
                       Default_Value : Boolean := False)
                      return Boolean
   is
      Result : constant String := Get_Value (Position, Value_Name);
   begin
      if Result = "" then
         return Default_Value;
      else
         return Boolean'Value (Result);
      end if;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Position      : Cursor;
                       Value_Name    : String;
                       Default_Value : Integer := 0)
                      return Integer
   is
      Result : constant String := Get_Value (Position, Value_Name);
   begin
      if Result = "" then
         return Default_Value;
      else
         return Integer'Value (Result);
      end if;
   end Get_Value;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= null;
   end Has_Element;

   ------------------
   -- Last_Project --
   ------------------

   function Last_Project return String is
      Position : constant Cursor :=
                   Get_Cursor ("/last_project");
   begin
      if Has_Element (Position) then
         return Get_Value (Position, "path", "");
      else
         return "";
      end if;
   end Last_Project;

   ------------------------
   -- Load_Configuration --
   ------------------------

   procedure Load_Configuration is
      use type Aquarius.Messages.Message_Level;
      Grammar      : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Manager.Get_Grammar ("aquarius-config");
      Config_File  : constant Aquarius.Programs.Program_Tree :=
        Aquarius.Loader.Load_From_File
        (Grammar    => Grammar,
         Project    =>
           Aquarius.Projects.New_Default_Project
           (Aquarius_Config_File),
           Interactor =>
           Aquarius.Interaction.Console.Console_Interactor,
         UI         =>
           Aquarius.UI.Console.Console_UI,
         Path       => Aquarius_Config_File);
      Errors       : Aquarius.Messages.Message_List;
   begin

      Config_File.Get_Messages (Errors);
      Aquarius.Messages.Console.Show_Messages (Errors);
      if Aquarius.Messages.Highest_Level (Errors) >
        Aquarius.Messages.Warning
      then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Errors in Aquarius configuration file");
         raise Configuration_Error;
      end if;

      Config := new Config_Item_Record'
        (Config_Type => Nested_Item,
         Parent      => null,
         Name        => Ada.Strings.Unbounded.To_Unbounded_String ("/"),
         Children    => Config_Item_Vector.To_Vector (0),
         Value       => Ada.Strings.Unbounded.Null_Unbounded_String);

      Build_Config (Config, Aquarius.Trees.Tree (Config_File));

   end Load_Configuration;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Item : Config_Item_Record) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Name);
   end Name;

   ----------------------------
   -- Path_Element_Not_Found --
   ----------------------------

   procedure Path_Element_Not_Found (Element : String) is
   begin
      raise Configuration_Error with
        "Path element '" & Element & "' not found";
   end Path_Element_Not_Found;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Position   : Cursor;
                        Value_Name : String;
                        Value      : String)
   is
      use Ada.Strings.Unbounded;
      Current : constant Cursor := Find_Child (Position, Value_Name);
   begin
      if Current = null then
         Position.Children.Append
           (new Config_Item_Record'(Config_Type => Value_Item,
                                    Parent      => Position,
                                    Name        =>
                                      To_Unbounded_String (Value_Name),
                                    Children    =>
                                      Config_Item_Vector.To_Vector (0),
                                    Value       =>
                                      To_Unbounded_String (Value)));
      else
         Current.Value := To_Unbounded_String (Value);
      end if;
      Write_Config;
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Position   : Cursor;
                        Value_Name : String;
                        Value      : Integer)
   is
   begin
      Set_Value (Position, Value_Name, Integer'Image (Value));
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Position   : Cursor;
                        Value_Name : String;
                        Value      : Boolean)
   is
   begin
      Set_Value (Position, Value_Name, Boolean'Image (Value));
   end Set_Value;

   -------------
   -- Warning --
   -------------

   procedure Warning (Message : String) is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            "configuration: warning: " & Message);
   end Warning;

   ------------------
   -- Write_Config --
   ------------------

   procedure Write_Config is
      use Ada.Text_IO;
      File : File_Type;

      Level : Positive_Count := 1;
      procedure Write (Pos : Cursor);

      -----------
      -- Write --
      -----------

      procedure Write (Pos : Cursor) is
      begin
         Set_Col (File, Level);
         Put (File, Ada.Strings.Unbounded.To_String (Pos.Name));
         case Pos.Config_Type is
            when Value_Item =>
               Put_Line (File, " = " &
                           Ada.Strings.Unbounded.To_String (Pos.Value) & ";");
            when Nested_Item =>
               New_Line (File);
               Set_Col (Level);
               Put_Line (File, "{");
               Level := Level + 2;
               for I in 1 .. Pos.Children.Last_Index loop
                  Write (Pos.Children.Element (I));
               end loop;
               Level := Level - 2;
               Set_Col (Level);
               Put_Line (File, "}");
         end case;
      end Write;

      Old_Config : constant String := Aquarius_Config_File & ".old";
      New_Config : constant String := Aquarius_Config_File & ".new";
   begin
      Create (File, Out_File, New_Config);
      Write (Config);
      Close (File);

      if Ada.Directories.Exists (Old_Config) then
         Ada.Directories.Delete_File (Old_Config);
      end if;
      Ada.Directories.Rename (Aquarius_Config_File, Old_Config);
      Ada.Directories.Rename (New_Config, Aquarius_Config_File);
   end Write_Config;

end Aquarius.Configuration;
