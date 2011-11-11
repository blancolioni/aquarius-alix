with Aquarius.Entries.Packages;

package body Aquarius.Projects.Package_View is

   type Package_View_Type is new Root_Project_View with null record;

   type Package_Tree_Type is new Root_Project_Tree with
      record
         Package_Entry : Aquarius.Entries.Table_Entry;
      end record;

   function Create_Tree
     (Project : not null access Aquarius_Project_Type'Class)
     return Aquarius.Trees.Tree;

   function New_Package_Tree_Node (Item : Aquarius.Entries.Table_Entry)
                                  return Aquarius.Trees.Tree;

   procedure Add_Package (Tree  : Aquarius.Trees.Tree;
                          Item  : Aquarius.Entries.Table_Entry);

   -----------------
   -- Add_Package --
   -----------------

   procedure Add_Package (Tree  : Aquarius.Trees.Tree;
                          Item  : Aquarius.Entries.Table_Entry)
   is
      use Aquarius.Entries;

      function Check_Entry (E : Table_Entry) return Aquarius.Trees.Tree;

      -----------------
      -- Check_Entry --
      -----------------

      function Check_Entry (E : Table_Entry) return Aquarius.Trees.Tree is
         use Aquarius.Entries.Packages;
         P : constant Table_Entry := E.Entry_Owner;
         T : Aquarius.Trees.Tree := null;
      begin

         if P /= null then
            T := Check_Entry (P);
         else
            T := Tree;
         end if;

         for I in 1 .. T.Child_Count loop
            if T.Child (I).Name = E.Name then
               return T.Child (I);
            end if;
         end loop;

         T.Add_Child (New_Package_Tree_Node (E));

         return T.Last_Child;

      end Check_Entry;

      T : constant Aquarius.Trees.Tree := Check_Entry (Item);
      pragma Unreferenced (T);

   begin
      null;
   end Add_Package;

   -------------------------
   -- Create_Package_View --
   -------------------------

   procedure Create_Package_View
     (Project : access Aquarius_Project_Type'Class)
   is
      Result : Package_View_Type;
   begin
      Result.Name    := Aquarius.Names.To_Aquarius_Name ("Package View");
      Result.Project := Aquarius_Project (Project);
      Result.View_Contents := Create_Tree (Project);
      Project.Views.Append (new Package_View_Type'(Result));
   end Create_Package_View;

   -----------------
   -- Create_Tree --
   -----------------

   function Create_Tree
     (Project : not null access Aquarius_Project_Type'Class)
     return Aquarius.Trees.Tree
   is
      use Project_Entries_Vector;
      Root : constant Aquarius.Trees.Tree :=
        New_Package_Tree_Node (null);
      It   : Cursor := Project.Entries.First;
   begin
      while Has_Element (It) loop
         if Aquarius.Entries.Packages.Is_Package (Element (It)) then
            Add_Package (Root, Element (It));
         end if;
         Next (It);
      end loop;

      return Root;
   end Create_Tree;

   ---------------------------
   -- New_Package_Tree_Node --
   ---------------------------

   function New_Package_Tree_Node (Item : Aquarius.Entries.Table_Entry)
                                  return Aquarius.Trees.Tree
   is
      use Ada.Strings.Unbounded;
      use type Aquarius.Entries.Table_Entry;
      Result : Aquarius.Trees.Tree;
   begin
      if Item = null then
         Result :=
           new Package_Tree_Type'
             (Aquarius.Trees.Root_Tree_Type with
              Label         => To_Unbounded_String ("Packages"),
              Target        => null,
              Package_Entry => null);
      else
         Result :=
           new Package_Tree_Type'
             (Aquarius.Trees.Root_Tree_Type with
              Label         => To_Unbounded_String (Item.Name),
              Target        =>
                Aquarius.Programs.Program_Tree (Item.Declaration),
              Package_Entry => Item);
      end if;
      return Result;
   end New_Package_Tree_Node;

end Aquarius.Projects.Package_View;
