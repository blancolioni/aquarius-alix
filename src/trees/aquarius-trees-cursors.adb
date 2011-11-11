package body Aquarius.Trees.Cursors is

   --  Get_Parent_From_List
   --  Finds a parent for a node that has been temporarily removed
   --  from a program tree to resolve an ambiguity
   function Get_Parent_From_List (Item : Cursor)
                                 return Tree;

   --  Get_Non_Temporary_Parent_From_List
   --  If Item currently sits at a node which has a copy in the
   --  list of parent trees, move to that.
   --  procedure Move_To_Non_Temporary (Item : in out Cursor);

   ----------------------
   -- Collapse_Parents --
   ----------------------

   procedure Collapse_Parents (Item : in out Cursor) is
   begin
      for Index in reverse 1 .. Item.Parents.Last_Index loop
         declare
            It : Parent_Child_Record renames Item.Parents.Element (Index);
         begin
            if It.Child.Temporary then
               --  it's not really parent-child, the child is a temporary
               --  copy of the parent, and we have to move its children
               --  over.
               declare
                  Child : Aquarius.Trees.Tree := It.Child.First_Child;
                  Next  : Tree;
               begin
                  while Child /= null loop
                     Next := Child.Right_Sibling;
                     It.Parent.Add_Child (Child);
                     Child := Next;
                  end loop;
               end;
            else
               It.Parent.Add_Child (It.Child);
            end if;
         end;
      end loop;
      Item.Parents.Set_Length (0);
   end Collapse_Parents;

   ------------------
   -- Copy_Parents --
   ------------------

   procedure Copy_Parents (From  : in     Cursor;
                           To    : in out Cursor)
   is
   begin
      To.Parents := From.Parents;
   end Copy_Parents;

   --------------
   -- Get_Left --
   --------------

   function Get_Left (Item : Cursor) return Cursor is
      Result : Cursor := Item;
   begin
      Move_Left (Result);
      return Result;
   end Get_Left;

   -----------------------
   -- Get_Left_Of_Child --
   -----------------------

   function Get_Left_Of_Child
     (Item  : Cursor;
      Child : access Root_Tree_Type'Class)
     return Cursor
   is
      Result : Cursor := Item;
   begin
      Result.Right_Tree := Tree (Child);
      Result.Off_Right  := False;
      return Result;
   end Get_Left_Of_Child;

   -----------------------------
   -- Get_Left_Of_First_Child --
   -----------------------------

   function Get_Left_Of_First_Child
     (Item : Cursor)
      return Cursor
   is
      Result : Cursor := Item;
   begin
      Move_To_Left_Of_First_Child (Result);
      return Result;
   end Get_Left_Of_First_Child;

   -------------------
   -- Get_Left_Tree --
   -------------------

   function Get_Left_Tree
     (Item : Cursor)
      return Tree
   is
   begin
      if Item.Off_Right then
         return Item.Right_Tree;
      else
         return Item.Right_Tree.Left_Sibling;
      end if;
   end Get_Left_Tree;

   --------------------------
   -- Get_Parent_From_List --
   --------------------------

   function Get_Parent_From_List
     (Item : Cursor)
     return Tree
   is
      use type Tree;
   begin
      for I in 1 .. Item.Parents.Last_Index loop
         declare
            It : Parent_Child_Record renames Item.Parents.Element (I);
         begin
            if It.Child = Get_Left_Tree (Item) or else
              It.Child = Get_Right_Tree (Item)
            then
               if It.Child.Temporary then
                  --  the child is actually a temporary version of the parent
                  return It.Parent.Parent;
               else
                  --  the child really is the child
                  return It.Parent;
               end if;
            end if;
         end;
      end loop;

      return null;
   end Get_Parent_From_List;

   ---------------
   -- Get_Right --
   ---------------

   function Get_Right (Item : Cursor) return Cursor is
      Result : Cursor := Item;
   begin
      Move_Right (Result);
      return Result;
   end Get_Right;

   -------------------------
   -- Get_Right_Of_Parent --
   -------------------------

   function Get_Right_Of_Parent
     (Item : Cursor)
      return Cursor
   is
      Result : Cursor := Item;
   begin
      Move_To_Right_Of_Parent (Result);
      return Result;
   end Get_Right_Of_Parent;

   --------------------
   -- Get_Right_Tree --
   --------------------

   function Get_Right_Tree
     (Item : Cursor)
      return Tree
   is
   begin
      if Is_Off_Right (Item) then
         return null;
      else
         return Item.Right_Tree;
      end if;
   end Get_Right_Tree;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree
     (Item : Cursor)
     return Tree
   is
   begin
      if Is_Off_Right (Item) then
         return Get_Left_Tree (Item);
      else
         return Get_Right_Tree (Item);
      end if;
   end Get_Tree;

   -----------
   -- Image --
   -----------

   function Image (Item : Cursor) return String is
   begin
      if Is_Off_Left (Item) then
         return "<::" & Get_Right_Tree (Item).Image & ">";
      elsif Is_Off_Right (Item) then
         return "<" & Get_Left_Tree (Item).Image & "::>";
      else
         return "<" & Get_Left_Tree (Item).Image & "::" &
         Get_Right_Tree (Item).Image & ">";
      end if;
   end Image;

   ----------------
   -- Is_At_Root --
   ----------------

   function Is_At_Root (Item : Cursor) return Boolean is
      T : constant Tree := Get_Tree (Item);
   begin
      if T.Parent /= null or else T.Foster_Parent /= null then
         return False;
      else
         return Get_Parent_From_List (Item) = null;
      end if;
   end Is_At_Root;

   -----------------
   -- Is_Off_Left --
   -----------------

   function Is_Off_Left (Item : Cursor) return Boolean is
   begin
      if Item.Off_Right then
         return False;
      else
         return Item.Right_Tree.Left_Sibling = null;
      end if;
   end Is_Off_Left;

   ------------------
   -- Is_Off_Right --
   ------------------

   function Is_Off_Right (Item : Cursor) return Boolean is
   begin
      return Item.Off_Right;
   end Is_Off_Right;

   ------------------
   -- Left_Of_Tree --
   ------------------

   function Left_Of_Tree
     (Item : access Root_Tree_Type'Class)
     return Cursor
   is
      Result : Cursor;
   begin
      Result.Parents.Set_Length (0);
      Result.Right_Tree := Tree (Item);
      Result.Off_Right  := False;
      return Result;
   end Left_Of_Tree;

   ---------------
   -- Move_Left --
   ---------------

   procedure Move_Left (Item : in out Cursor) is
   begin
      if Is_Off_Left (Item) then
         raise Cursor_Error with
           "attempt to move left from off-left position";
      elsif Item.Off_Right then
         Item.Off_Right := False;
      else
         Item.Right_Tree := Item.Right_Tree.Left_Sibling;
      end if;
   end Move_Left;

   ----------------
   -- Move_Right --
   ----------------

   procedure Move_Right (Item : in out Cursor) is
   begin
      if Is_Off_Right (Item) then
         raise Cursor_Error with
           "attempt to move right from off-right position";
      elsif Item.Right_Tree.Right_Sibling = null then
         Item.Off_Right := True;
      else
         Item.Right_Tree := Item.Right_Tree.Right_Sibling;
      end if;
   end Move_Right;

   ---------------------------------
   -- Move_To_Left_Of_First_Child --
   ---------------------------------

   procedure Move_To_Left_Of_First_Child
     (Item : in out Cursor)
   is
   begin
      if Is_Off_Right (Item) then
         raise Cursor_Error with
           "attempt to get first child from off-right position";
      elsif Item.Right_Tree.First_Child = null then
         raise Cursor_Error with
           "attempt to get first child from childless position";
      else
         Item.Right_Tree := Item.Right_Tree.First_Child;
         Item.Off_Right := False;
      end if;
   end Move_To_Left_Of_First_Child;

   -----------------------------
   -- Move_To_Left_Of_Parent --
   -----------------------------

   procedure Move_To_Left_Of_Parent (Item : in out Cursor) is
      Parent : constant Tree := Get_Tree (Item).Parent;
   begin
      if Parent = null then
         raise Cursor_Error with
           "attempt to move to left of null parent";
      else
         Item.Right_Tree := Parent;
         Item.Off_Right  := False;
      end if;

   end Move_To_Left_Of_Parent;

   -----------------------------
   -- Move_To_Right_Of_Parent --
   -----------------------------

   procedure Move_To_Right_Of_Parent (Item : in out Cursor) is
      Parent : Tree := Get_Tree (Item).Parent;
   begin
      if Parent = null then
         Parent := Get_Tree (Item).Foster_Parent;
      end if;

      if Parent = null then
         Parent := Get_Parent_From_List (Item);
      end if;

      if Parent = null then
         raise Cursor_Error with
           "attempt to move to right of null parent";
      else
         if Parent.Right_Sibling = null then
            Item.Right_Tree := Parent;
            Item.Off_Right  := True;
         else
            Item.Right_Tree := Parent.Right_Sibling;
            Item.Off_Right  := False;
         end if;

         --  20040221: Fraser
         --  If we're now at a node that temporarily has no parent
         --  due to an ambiguity resolution in progress, and if the
         --  node is really a temporary copy of its real parent, we
         --  want to move into the real parent, otherwise we'll think
         --  that the node has no siblings.

         --  20081129: Fraser: I don't understand the above comment
         --  (it's to do with non-LL(1) parsing), and what I don't
         --  understand, I destroy!
--           Parent := Get_Tree (Item).Parent;
--           if Parent = null then
--              Move_To_Non_Temporary (Item);
--           end if;

      end if;

   end Move_To_Right_Of_Parent;

   -------------------
   -- Right_Of_Tree --
   -------------------

   function Right_Of_Tree
     (Item : access Root_Tree_Type'Class)
     return Cursor
   is
      Result : Cursor;
   begin
      Result.Parents.Set_Length (0);
      Result.Right_Tree := Tree (Item);
      Result.Off_Right  := False;
      Move_Right (Result);
      return Result;
   end Right_Of_Tree;

   -----------------
   -- Set_Parents --
   -----------------

   procedure Set_Parents
     (Item         : in out Cursor;
      Derived_From : in     Cursor;
      New_Parent   : in     Tree;
      New_Child    : in     Tree)
   is
      Original_Parents : constant Parent_Child_Vectors.Vector :=
        Item.Parents;
   begin
      Item.Parents :=
        Parent_Child_Vectors.To_Vector ((New_Parent, New_Child), 1);
      Item.Parents.Append (Derived_From.Parents);
      Item.Parents.Append (Original_Parents);
   end Set_Parents;

   -----------
   -- Show --
   -----------

   function Show (Item : Cursor) return String is
   begin
      if Is_Off_Left (Item) then
         return "<::" & Get_Right_Tree (Item).Name & ">";
      elsif Is_Off_Right (Item) then
         return "<" & Get_Left_Tree (Item).Name & "::>";
      else
         return "<" & Get_Left_Tree (Item).Name & "::" &
           Get_Right_Tree (Item).Name & ">";
      end if;
   end Show;

end Aquarius.Trees.Cursors;
