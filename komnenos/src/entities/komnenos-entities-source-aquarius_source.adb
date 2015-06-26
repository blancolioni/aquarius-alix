with Ada.Text_IO;

with Aquarius.Programs.Arrangements;
with Aquarius.Rendering;
with Aquarius.Themes;
with Aquarius.Trees.Cursors;

with Komnenos.Fragments.Rendering;
with Komnenos.Fragments.Source;
with Komnenos.UI;

package body Komnenos.Entities.Source.Aquarius_Source is

   type Root_Aquarius_Source_Entity is
     new Root_Source_Entity_Reference with
      record
         Top_Level        : Boolean;
         Compilation_Unit : Aquarius.Programs.Program_Tree;
         Entity_Spec      : Aquarius.Programs.Program_Tree;
         Entity_Body      : Aquarius.Programs.Program_Tree;
      end record;

   overriding function Top_Level
     (Entity : Root_Aquarius_Source_Entity)
      return Boolean
   is (Entity.Top_Level);

   overriding procedure Select_Entity
     (Entity : Root_Aquarius_Source_Entity;
      Table  : access Entity_Table_Interface'Class;
      Parent : access Entity_Visual'Class;
      Visual : access Entity_Visual'Class;
      Offset : Natural);

   function Get_Key (File_Name : String;
                     Line      : Natural;
                     Column    : Natural;
                     Name      : String)
                     return String;

   procedure Log_Tree (Tree : Aquarius.Programs.Program_Tree);

   -----------------------------------
   -- Create_Aquarius_Source_Entity --
   -----------------------------------

   function Create_Aquarius_Source_Entity
     (Table            : not null access Entity_Table_Interface'Class;
      Name             : String;
      File_Name        : String;
      Class            : String;
      Line             : Natural;
      Column           : Natural;
      Top_Level        : Boolean;
      Compilation_Unit : Aquarius.Programs.Program_Tree;
      Entity_Spec      : Aquarius.Programs.Program_Tree;
      Entity_Body      : Aquarius.Programs.Program_Tree)
      return Entity_Reference
   is
      Key : constant String := Get_Key (File_Name, Line, Column, Name);
   begin
      if not Table.Exists (Key) then
         declare
            Entity : Root_Aquarius_Source_Entity;
            Result : Entity_Reference;
         begin
            Entity.Create (Name, File_Name, Class, Line, Column);
            Entity.Top_Level := Top_Level;
            Entity.Compilation_Unit := Compilation_Unit;
            Entity.Entity_Spec := Entity_Spec;
            Entity.Entity_Body := Entity_Body;
            Result := new Root_Aquarius_Source_Entity'(Entity);
            Table.Add_Entity (Key, Result);
            return Result;
         end;
      else
         return Table.Get (Key);
      end if;
   end Create_Aquarius_Source_Entity;

   ----------------------------
   -- Find_Entity_Containing --
   ----------------------------

   function Find_Entity_Containing
     (Table     : not null access Entity_Table_Interface'Class;
      Location  : File_Location)
      return Entity_Reference
   is
   begin
      return Table.Get_Reference (Location);
   end Find_Entity_Containing;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (File_Name : String;
                     Line      : Natural;
                     Column    : Natural;
                     Name      : String)
                     return String
   is
   begin
      return File_Name & Integer'Image (-Line)
        & Integer'Image (-Column)
        & "-" & Name;
   end Get_Key;

   --------------
   -- Log_Tree --
   --------------

   procedure Log_Tree (Tree : Aquarius.Programs.Program_Tree) is

      procedure Log (T : Aquarius.Programs.Program_Tree;
                     Start : Ada.Text_IO.Count);

      ---------
      -- Log --
      ---------

      procedure Log (T : Aquarius.Programs.Program_Tree;
                     Start : Ada.Text_IO.Count)
      is
         use Ada.Text_IO;
         New_Start : Count := Start;
      begin
         if T.Name /= "" then
            Set_Col (Start);
            Put_Line (T.Image);
            New_Start := Start + 2;
         end if;
         for I in 1 .. T.Child_Count loop
            Log (T.Program_Child (I), New_Start);
         end loop;
      end Log;

   begin
      Log (Tree, 1);
   end Log_Tree;

   -------------------
   -- Select_Entity --
   -------------------

   overriding procedure Select_Entity
     (Entity : Root_Aquarius_Source_Entity;
      Table  : access Entity_Table_Interface'Class;
      Parent : access Entity_Visual'Class;
      Visual : access Entity_Visual'Class;
      Offset : Natural)
   is
      use Ada.Strings.Unbounded;
      use type Aquarius.Programs.Program_Tree;
      Fragment : constant Komnenos.Fragments.Fragment_Type :=
                   (if Visual = null
                    then Komnenos.Fragments.Source.New_Source_Fragment
                      (Title => To_String (Entity.Description),
                       Path  => To_String (Entity.File_Name))
                    else Komnenos.Fragments.Fragment_Type (Visual));
      Renderer : constant Aquarius.Rendering.Aquarius_Renderer :=
                   Komnenos.Fragments.Rendering.New_Fragment_Renderer
                     (Fragment, Table);
      Program  : constant Aquarius.Programs.Program_Tree :=
                   (if Entity.Entity_Body /= null
                    then Entity.Entity_Body
                    else Entity.Entity_Spec);
   begin
      Ada.Text_IO.Put_Line (Program.Image);

      Fragment.Set_Entity_Key (Key (Entity));

      if False then
         Log_Tree (Program);
      end if;

      Aquarius.Programs.Arrangements.Arrange
        (Program,
         Line_Length => Fragment.Width / 10);

      Renderer.Set_Theme (Aquarius.Themes.Active_Theme);

      Aquarius.Programs.Arrangements.Render
        (Program   => Program,
         Renderer  => Renderer,
         Point     => Aquarius.Trees.Cursors.Left_Of_Tree (Entity.Entity_Spec),
         Partial   => "");

      if Visual = null then
         Komnenos.UI.Current_UI.Place_Fragment
           (Parent, Offset, Fragment);
      end if;

   end Select_Entity;

   ---------------------
   -- Set_Entity_Body --
   ---------------------

   procedure Set_Entity_Body
     (Entity : Entity_Reference;
      Entity_Body : Aquarius.Programs.Program_Tree)
   is
   begin
      Root_Aquarius_Source_Entity (Entity.all).Entity_Body := Entity_Body;
   end Set_Entity_Body;

end Komnenos.Entities.Source.Aquarius_Source;
