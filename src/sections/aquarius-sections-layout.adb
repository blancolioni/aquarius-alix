package body Aquarius.Sections.Layout is

   procedure Create_Room
     (Layout : in out Section_Layout;
      X, Y   : in out Integer;
      Width  : in     Natural;
      Height : in     Natural);

   procedure Move
     (Layout   : in out Section_Layout;
      Section  : Aquarius_Section;
      DX, DY   : Integer);

   function Point_Contained
     (Layout : Section_Layout;
      X, Y   : Integer;
      Item   : Section_Entry)
      return Boolean;

   -----------------
   -- Create_Room --
   -----------------

   procedure Create_Room
     (Layout : in out Section_Layout;
      X, Y   : in out Integer;
      Width  : in     Natural;
      Height : in     Natural)
   is
      Candidate : Boolean := True;
      DX, DY    : Integer := 0;
      Section   : Aquarius_Section;
   begin
      for Item of Layout.Entries loop
         if Point_Contained (Layout, X, Y, Item) then
            DX := Item.X + Item.Section.Render_Width - X;
            DY := Item.Y + Item.Section.Render_Height - Y;
            if DX < DY then
               X := X + DX;
            else
               Y := Y + DY;
            end if;
            exit;
         end if;
      end loop;

      while Candidate loop
         Candidate := False;
         for Item of Layout.Entries loop
            if Point_Contained (Layout, X, Y, Item) then
               DX := X + Width - Item.X;
               DY := Y + Height - Item.Y;
               if DX < DY then
                  DY := 0;
               else
                  DX := 0;
               end if;
               Candidate := True;
               Section   := Item.Section;
               exit;
            end if;
         end loop;

         if Candidate then
            Move (Layout, Section, DX, DY);
         end if;
      end loop;
   end Create_Room;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise
     (Layout        : in out Section_Layout;
      Width, Height : Positive)
   is
   begin
      Layout.Width := Width;
      Layout.Height := Height;
   end Initialise;

   ----------
   -- Move --
   ----------

   procedure Move
     (Layout   : in out Section_Layout;
      Section  : Aquarius_Section;
      DX, DY   : Integer)
   is
      use Section_Entry_Vectors;
   begin
      for It in Layout.Entries.Iterate loop
         if Element (It).Section = Section then
            declare
               Item  : Section_Entry := Element (It);
               New_X : Integer := Item.X + DX;
               New_Y : Integer := Item.Y + DY;
            begin
               Create_Room (Layout => Layout,
                            X      => New_X,
                            Y      => New_Y,
                            Width  => Item.Section.Render_Width,
                            Height => Item.Section.Render_Height);
               Item.X := New_X;
               Item.Y := New_Y;
               Layout.Entries.Replace_Element (It, Item);
               return;
            end;
         end if;
      end loop;
   end Move;

   ---------------------
   -- Point_Contained --
   ---------------------

   function Point_Contained
     (Layout : Section_Layout;
      X, Y   : Integer;
      Item   : Section_Entry)
      return Boolean
   is
      pragma Unreferenced (Layout);
   begin
      return X in Item.X .. Item.X + Item.Section.Render_Width
        and then Y in Item.Y .. Item.Y + Item.Section.Render_Height;
   end Point_Contained;

   ------------
   -- Render --
   ------------

   procedure Render
     (Layout       : Section_Layout;
      X_Min, X_Max : Integer;
      Y_Min, Y_Max : Integer;
      Renderer     : not null access
        procedure (Section : Aquarius.Sections.Aquarius_Section;
                   X, Y    : Integer))
   is
   begin
      for Index of Layout.Horizontal loop
         declare
            Ent : Section_Entry renames Layout.Entries (Index);
         begin
            exit when Ent.X > X_Max;
            if Ent.X >= X_Min - Ent.Section.Render_Width
              and then Ent.Y >= Y_Min - Ent.Section.Render_Height
              and then Ent.Y <= Y_Max
            then
               Renderer (Ent.Section, Ent.X, Ent.Y);
            end if;
         end;
      end loop;
   end Render;

   ---------------------
   -- Render_Overview --
   ---------------------

   procedure Render_Overview
     (Layout : Section_Layout;
      Renderer     : not null access
        procedure (Section : Aquarius.Sections.Aquarius_Section;
                   X, Y    : Integer))
   is
   begin
      for Ent of Layout.Entries loop
         Renderer (Ent.Section, Ent.X, Ent.Y);
      end loop;
   end Render_Overview;

   ------------------
   -- Show_Section --
   ------------------

   procedure Show_Section
     (Layout  : in out Section_Layout;
      Section : Aquarius.Sections.Aquarius_Section;
      Hint_X  : Integer;
      Hint_Y  : Integer)
   is
      Layout_X : Integer := Hint_X;
      Layout_Y : Integer := Hint_Y;
   begin
      Create_Room (Layout, Layout_X, Layout_Y,
                   Section.Render_Width,
                   Section.Render_Height);
      Layout.Entries.Append ((Section, Layout_X, Layout_Y));
      declare
         use List_Of_Section_Indices;
         Inserted : Boolean := False;
         Index     : constant Positive := Layout.Entries.Last_Index;
         It        : Cursor :=
                       Layout.Horizontal.First;
      begin
         while not Inserted and then Has_Element (It) loop
            declare
               Index : constant Positive := Element (It);
               X     : constant Integer :=
                         Layout.Entries (Index).X;
            begin
               if X > Layout_X then
                  Layout.Horizontal.Insert (It, Index);
                  Inserted := True;
               else
                  Next (It);
               end if;
            end;
         end loop;

         if not Inserted then
            Layout.Horizontal.Append (Index);
         end if;
      end;
   end Show_Section;

end Aquarius.Sections.Layout;
