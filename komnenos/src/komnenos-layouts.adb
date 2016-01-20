package body Komnenos.Layouts is

   type Direction is (Up, Down, Left, Right);

   type Available_Directions is array (Direction) of Boolean;

   procedure Clear_Space
     (Layout      : in out Root_Layout_Type'Class;
      Fragment    : Komnenos.Fragments.Fragment_Type;
      Available   : Available_Directions);

   function Overlaps
     (X1, Y1 : Integer;
      W1, H1 : Positive;
      X2, Y2 : Integer;
      W2, H2 : Positive)
      return Boolean;

   function Overlaps
     (Fragment : Komnenos.Fragments.Fragment_Type;
      X, Y     : Integer;
      W, H     : Positive)
      return Boolean
   is (Overlaps (Fragment.X, Fragment.Y, Fragment.Width, Fragment.Height,
                 X, Y, W, H));
   pragma Unreferenced (Overlaps);

   function Overlaps
     (Fragment_1, Fragment_2 : Komnenos.Fragments.Fragment_Type)
     return Boolean
   is (Overlaps (Fragment_1.X, Fragment_1.Y,
                 Fragment_1.Width, Fragment_1.Height,
                 Fragment_2.X, Fragment_2.Y,
                 Fragment_2.Width, Fragment_2.Height));

   -----------------
   -- Clear_Space --
   -----------------

   procedure Clear_Space
     (Layout      : in out Root_Layout_Type'Class;
      Fragment    : Komnenos.Fragments.Fragment_Type;
      Available   : Available_Directions)
   is
      use type Komnenos.Fragments.Fragment_Type;
      Moved_Fragment : Komnenos.Fragments.Fragment_Type := null;
      Need_Move      : Boolean := False;
      Possible       : Available_Directions := Available;
   begin
      for R of Layout.Items loop
         if R /= Fragment and then Overlaps (Fragment, R) then
            if Possible (Left) then
               R.Set_Position (Fragment.X - R.Width - Margin, R.Y);
               Possible (Right) := False;
            elsif Possible (Up) and then R.Y < Fragment.Y
              and then Fragment.Y - R.Height - Margin > 0
            then
               R.Set_Position (R.X, Fragment.Y - R.Height - Margin);
               Possible (Down) := False;
            elsif Possible (Down) and then R.Y > Fragment.Y then
               R.Set_Position (R.X, Fragment.Y + Fragment.Height + Margin);
            else
               R.Set_Position (Fragment.X + Fragment.Width + Margin, R.Y);
            end if;

            Moved_Fragment := R;
            Need_Move := True;
            exit;
         end if;
      end loop;

      if Need_Move then
         Clear_Space (Layout, Moved_Fragment, Possible);
         Clear_Space (Layout, Fragment, Available);
         Layout.Item_Moved (Moved_Fragment);
      end if;
   end Clear_Space;

   -----------------
   -- From_Config --
   -----------------

   overriding procedure From_Config
     (Layout : not null access Root_Layout_Type;
      Config : Tropos.Configuration)
   is
   begin
      for Child_Config of Config loop
         declare
            subtype Base is
              Komnenos.Session_Objects.Session_Object_Interface'Class;
            Session_Fragment : constant access Base :=
                                 Komnenos.Session_Objects.Read_Config
                                   (Child_Config);
            Fragment         : constant Komnenos.Fragments.Fragment_Type :=
                                 Komnenos.Fragments.Fragment_Type
                                   (Session_Fragment);
         begin
            Layout.Items.Append (Fragment);
            Root_Layout_Type'Class (Layout.all).Item_Placed (Fragment);
         end;
      end loop;
   end From_Config;

   -----------------
   -- Full_Height --
   -----------------

   function Full_Height (Layout : Root_Layout_Type) return Natural is
   begin
      return Layout.Full_Height;
   end Full_Height;

   ----------------
   -- Full_Width --
   ----------------

   function Full_Width (Layout : Root_Layout_Type) return Natural is
   begin
      return Layout.Full_Width;
   end Full_Width;

   ---------------
   -- Move_Item --
   ---------------

   procedure Move_Item
     (Layout    : in out Root_Layout_Type'Class;
      Fragment  : Komnenos.Fragments.Fragment_Type)
   is
   begin
      Clear_Space
        (Layout, Fragment,
         Available => (others => True));
      Layout.Item_Moved (Fragment);
   end Move_Item;

   --------------
   -- Overlaps --
   --------------

   function Overlaps
     (X1, Y1 : Integer;
      W1, H1 : Positive;
      X2, Y2 : Integer;
      W2, H2 : Positive)
      return Boolean
   is
   begin
      return not (X1 + W1 < X2
                  or else X2 + W2 < X1
                  or else Y1 + H1 < Y2
                  or else Y2 + H2 < Y1);
   end Overlaps;

   ----------------
   -- Place_Item --
   ----------------

   procedure Place_Item
     (Layout    : in out Root_Layout_Type'Class;
      Fragment  : Komnenos.Fragments.Fragment_Type;
      Reference : Komnenos.Fragments.Fragment_Type;
      Offset    : Integer)
   is
   begin
      Fragment.Set_Position
        (Reference.X + Reference.Width + Margin,
         Reference.Y + Offset);

      Clear_Space
        (Layout, Fragment,
         Available => (Left => False, others => True));
      Place_Item (Layout, Fragment);
   end Place_Item;

   ----------------
   -- Place_Item --
   ----------------

   procedure Place_Item
     (Layout    : in out Root_Layout_Type'Class;
      Fragment  : Komnenos.Fragments.Fragment_Type)
   is
      X : Integer := Fragment.X;
      Y : constant Integer := Fragment.Y;
      Moved : Boolean := True;
   begin
      while Moved loop
         Moved := False;
         for R of Layout.Items loop
            if Overlaps (R.X, R.Y, R.Width, R.Height,
                         X, Y, Fragment.Width, Fragment.Height)
            then
               X := R.X + R.Width + Margin;
               Moved := True;
            end if;
         end loop;
      end loop;

      Fragment.Set_Position (X, Y);
      Layout.Items.Append (Fragment);
      Layout.Item_Placed (Fragment);

   end Place_Item;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Layout  : Root_Layout_Type'Class;
      Process : not null access
        procedure (Fragment : Komnenos.Fragments.Fragment_Type))
   is
   begin
      for Fragment of Layout.Items loop
         Process (Fragment);
      end loop;
   end Scan;

   -------------------
   -- Set_Full_Size --
   -------------------

   procedure Set_Full_Size
     (Layout      : in out Root_Layout_Type;
      Full_Width  : Natural;
      Full_Height : Natural)
   is
   begin
      Layout.Full_Width := Full_Width;
      Layout.Full_Height := Full_Height;
   end Set_Full_Size;

   ----------------------
   -- Set_Visible_Area --
   ----------------------

   procedure Set_Visible_Area
     (Layout        : in out Root_Layout_Type;
      Left, Top     : Integer;
      Width, Height : Positive)
   is
   begin
      Layout.Visible_Left := Left;
      Layout.Visible_Top := Top;
      Layout.Visible_Width := Width;
      Layout.Visible_Height := Height;
   end Set_Visible_Area;

   ---------------
   -- To_Config --
   ---------------

   overriding procedure To_Config
     (Layout : Root_Layout_Type;
      Config : in out Tropos.Configuration)
   is
   begin
      for Fragment of Layout.Items loop
         declare
            Fragment_Config : Tropos.Configuration :=
                                Tropos.New_Config (Fragment.Config_Name);
         begin
            Fragment.To_Config (Fragment_Config);
            Config.Add (Fragment_Config);
         end;
      end loop;
   end To_Config;

   --------------------
   -- Visible_Height --
   --------------------

   function Visible_Height
     (Layout : Root_Layout_Type'Class)
      return Positive
   is
   begin
      return Layout.Visible_Height;
   end Visible_Height;

   ------------------
   -- Visible_Left --
   ------------------

   function Visible_Left
     (Layout : Root_Layout_Type'Class)
      return Integer
   is
   begin
      return Layout.Visible_Left;
   end Visible_Left;

   -----------------
   -- Visible_Top --
   -----------------

   function Visible_Top
     (Layout : Root_Layout_Type'Class)
      return Integer
   is
   begin
      return Layout.Visible_Top;
   end Visible_Top;

   -------------------
   -- Visible_Width --
   -------------------

   function Visible_Width
     (Layout : Root_Layout_Type'Class)
      return Positive
   is
   begin
      return Layout.Visible_Width;
   end Visible_Width;

end Komnenos.Layouts;
