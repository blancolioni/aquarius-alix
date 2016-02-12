package Aquarius.Trees.Cursors is

   Cursor_Error : exception;

   type Cursor is private;

   function Left_Of_Tree (Item : access Root_Tree_Type'Class) return Cursor;
   function Right_Of_Tree (Item : access Root_Tree_Type'Class) return Cursor;

   function Is_Off_Right (Item : Cursor) return Boolean;
   function Is_Off_Left  (Item : Cursor) return Boolean;

   function Is_At_Root (Item : Cursor) return Boolean;

   function Get_Left_Tree
     (Item : Cursor)
     return Tree;

   function Get_Right_Tree
     (Item : Cursor)
     return Tree;

   function Get_Tree
     (Item : Cursor)
     return Tree;

   function Get_Left        (Item : Cursor) return Cursor;
   function Get_Right       (Item : Cursor) return Cursor;

   procedure Move_Left           (Item : in out Cursor);
   procedure Move_Right          (Item : in out Cursor);

   function Get_Left_Of_First_Child
     (Item : Cursor)
      return Cursor;

   function Get_Left_Of_Child
     (Item  : Cursor;
      Child : access Root_Tree_Type'Class)
     return Cursor;

   function Get_Right_Of_Parent
     (Item : Cursor)
      return Cursor;

   procedure Move_To_Left_Of_First_Child (Item : in out Cursor);
   procedure Move_To_Right_Of_Parent     (Item : in out Cursor);
   procedure Move_To_Left_Of_Parent      (Item : in out Cursor);

   function Image (Item : Cursor) return String;
   function Show (Item : Cursor) return String;

private

   type Cursor is
      record
         Right_Tree : Tree;
         Off_Right  : Boolean;
      end record;

   pragma Inline (Get_Left_Tree);
   pragma Inline (Get_Right_Of_Parent);
   pragma Inline (Is_At_Root);

end Aquarius.Trees.Cursors;
