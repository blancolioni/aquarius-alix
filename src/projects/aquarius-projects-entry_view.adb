package body Aquarius.Projects.Entry_View is

   type Entry_View_Type is new Root_Project_View with null record;

   type Entry_Tree_Type is new Root_Project_Tree with
      record
         Associated_Entry : Aquarius.Entries.Table_Entry;
      end record;

   overriding
   procedure Reload (View : in out Entry_View_Type) is null;

   function Create_Tree (Project : access Aquarius_Project_Type'Class)
                        return Aquarius.Trees.Tree;

   function New_Entry_Tree_Node (Item : Aquarius.Entries.Table_Entry)
                                return Aquarius.Trees.Tree;

   ----------------------
   -- Create_Entry_View --
   ----------------------

   procedure Create_Entry_View
     (Project : access Aquarius_Project_Type'Class)
   is
      Result : Entry_View_Type;
   begin
      Result.Name    := Aquarius.Names.To_Aquarius_Name ("Entry View");
      Result.Project := Aquarius_Project (Project);
      Result.View_Contents := Create_Tree (Project);
      Project.Views.Append (new Entry_View_Type'(Result));
   end Create_Entry_View;

   -----------------
   -- Create_Tree --
   -----------------

   function Create_Tree (Project : access Aquarius_Project_Type'Class)
                        return Aquarius.Trees.Tree
   is
      use Project_Entries_Vector;
      Root : constant Aquarius.Trees.Tree := New_Entry_Tree_Node (null);
      It   : Cursor := Project.Entries.First;
   begin

      while Has_Element (It) loop
         Root.Add_Child (New_Entry_Tree_Node (Element (It)));
         Next (It);
      end loop;
      return Root;
   end Create_Tree;

   ------------------------
   -- New_Entry_Tree_Node --
   ------------------------

   function New_Entry_Tree_Node (Item : Aquarius.Entries.Table_Entry)
                               return Aquarius.Trees.Tree
   is
      use Ada.Strings.Unbounded;
      use Aquarius.Entries;
      Result : Aquarius.Trees.Tree;
   begin
      if Item = null then
         Result := new Entry_Tree_Type'(Aquarius.Trees.Root_Tree_Type with
                                        Label =>
                                          To_Unbounded_String ("Entries"),
                                        Target => null,
                                        Associated_Entry => null);
      else
         Result := new Entry_Tree_Type'
           (Aquarius.Trees.Root_Tree_Type with
            Label =>
              To_Unbounded_String
                (Item.Display_Name & " (" &
                 Aquarius.Source.Show
                   (Item.Declaration.Get_Location) &
                 ")"),
            Target =>
              Aquarius.Programs.Program_Tree
                (Item.Declaration),
            Associated_Entry => Item);

         declare
            Dec : constant Aquarius.Programs.Program_Tree :=
              Aquarius.Programs.Program_Tree (Item.Declaration);
            Loc : constant Aquarius.Source.Source_Position :=
              Dec.Get_Location;
         begin
            Result.Set_Location (Loc);
         end;
      end if;

      return Result;
   end New_Entry_Tree_Node;

end Aquarius.Projects.Entry_View;
