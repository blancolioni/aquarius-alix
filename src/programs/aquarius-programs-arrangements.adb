with Aquarius.Formats;
with Aquarius.Layout;
with Aquarius.Messages;

package body Aquarius.Programs.Arrangements is

   use Aquarius.Layout;
   use Aquarius.Formats;

   type Arrangement_Context is
      record
         Need_New_Line     : Boolean             := False;
         New_Line_Priority : Rule_Priority       := 1;
         Got_New_Line      : Boolean             := False;
         First_On_Line     : Boolean             := True;
         Need_Space        : Boolean             := False;
         No_Space          : Boolean             := False;
         Cancel_Indent     : Boolean             := False;
         Message_Level     : Aquarius.Messages.Message_Level :=
           Aquarius.Messages.No_Message;
         Space_Priority    : Rule_Priority       := 1;
         Current_Line      : Count               := 1;
         Current_Column    : Count               := 1;
         Current_Position  : Count               := 0;
         Current_Indent    : Count               := 1;
         Right_Margin      : Positive            := 72;
         Overflow          : Boolean             := False;
         Overflow_Line     : Count               := 0;
         User_Text_Length  : Count               := 0;
         User_Cursor       : Aquarius.Trees.Cursors.Cursor;
      end record;

   procedure Arrange (Item    : in     Program_Tree;
                      Context : in out Arrangement_Context);

   procedure Arrange_Terminal (Item    : in     Program_Tree;
                               Context : in out Arrangement_Context);

   procedure Arrange_Non_Terminal (Item    : in     Program_Tree;
                                   Context : in out Arrangement_Context);

   function Before_Point (Location : Program_Tree;
                          Point    : Aquarius.Trees.Cursors.Cursor)
                         return Boolean;

   --  Repair: look for terminals that are over the margin and repair them.

--     procedure Repair (Item            : in Program_Tree;
--                       Right_Margin    : in Positive);

   -------------
   -- Arrange --
   -------------

   procedure Arrange (Item    : in     Program_Tree;
                      Context : in out Arrangement_Context)
   is
      use type Aquarius.Messages.Message_Level;
      Old_Message_Level : constant Aquarius.Messages.Message_Level :=
        Context.Message_Level;
   begin
      if Item.Has_Messages and then
        Item.Get_Message_Level > Old_Message_Level
      then
         Context.Message_Level := Item.Get_Message_Level;
      end if;

      Item.Set_Inherited_Message_Level (Context.Message_Level);

      if Item.Is_Terminal then
         Arrange_Terminal (Item, Context);
      else
         Arrange_Non_Terminal (Item, Context);
      end if;

      Context.Message_Level := Old_Message_Level;

   end Arrange;

   -------------
   -- Arrange --
   -------------

   procedure Arrange (Item        : in Program_Tree;
                      Line_Length : in Positive      := 72)
   is
      Context : Arrangement_Context;
   begin
      Context.Right_Margin := Line_Length;
      Arrange (Item, Context);
   end Arrange;

   -------------
   -- Arrange --
   -------------

   procedure Arrange (Item           : in Program_Tree;
                      Point          : in Aquarius.Trees.Cursors.Cursor;
                      Partial_Length : in Natural;
                      Line_Length    : in Positive      := 72)
   is
      Context : Arrangement_Context;
   begin
      Context.Right_Margin := Line_Length;
      Context.User_Text_Length := Count (Partial_Length);
      Context.User_Cursor := Point;
      Arrange (Item, Context);
   end Arrange;

   --------------------------
   -- Arrange_Non_Terminal --
   --------------------------

   procedure Arrange_Non_Terminal (Item    : in     Program_Tree;
                                   Context : in out Arrangement_Context)
   is
      Format       : constant Aquarius_Format :=
        Item.Syntax.Get_Format;
      Rules        : constant Immediate_Rules := Formats.Rules (Format);
      Child_Indent : constant Indentation_Offset :=
        Indent_Child (Format);
      Old_Context  : constant Arrangement_Context := Context;
   begin

      if Item.Vertical_Gap_Before > 0 then
         Context.Current_Line :=
           Context.Current_Line + Item.Vertical_Gap_Before;
         Context.Current_Column := 1;
      end if;

      if Enabled (Rules.New_Line_Before) then
         Context.Need_New_Line := True;
      end if;

      if Item.Soft_New_Line then
         Context.Need_New_Line := True;
         Context.Current_Indent := Context.Current_Indent + 2;
      end if;

      Context.Current_Indent :=
        Count (Indentation_Offset (Context.Current_Indent)
                 + Child_Indent + Indent (Format, Before));

      Item.Start_Position := (Context.Current_Line,
                              Context.Current_Column);
      for I in 1 .. Item.Child_Count loop
         Arrange (Item.Program_Child (I), Context);
      end loop;

      if Context.Overflow and then not Item.Overflow_Checked then
         declare
            Children : constant Array_Of_Program_Trees :=
              Item.Direct_Children (Skip_Separators => False);
         begin
            for I in reverse Children'Range loop

               if Children (I).Is_Separator
                 and then Children (I).Has_Space_After
               then
                  if  Positive (Children (I).Layout_End_Column) <=
                    Context.Right_Margin
                  then
                     Children (I).Set_Separator_New_Line;
                     Context := Old_Context;
                     Arrange (Item, Context);
                     return;
                  end if;
               else
                  declare
                     Child_Format : constant Aquarius_Format :=
                       Children (I).Syntax.Get_Format;
                     Child_Rules  : constant Immediate_Rules :=
                       Formats.Rules (Child_Format);
                  begin
                     if Enabled (Child_Rules.Soft_New_Line_Before) then
                        Children (I).Set_Soft_New_Line;
                        Context := Old_Context;
                        Arrange (Item, Context);
                        return;
                     end if;
                  end;
               end if;
            end loop;

            --  we checked, and got nothing, so don't check again
            Item.Overflow_Checked := True;
         end;

      end if;

      Context.Current_Indent :=
        Count (Indentation_Offset (Context.Current_Indent)
                 - Child_Indent + Indent (Format, After));

      if Item.Soft_New_Line then
         Context.Current_Indent := Context.Current_Indent - 2;
      end if;

      if Enabled (Rules.New_Line_After) then
         Context.Need_New_Line := True;
      end if;

      --  turn off the overflow flag if we've crossed lines
      if Item.Start_Position.Line /= Context.Overflow_Line then
         Context.Overflow := False;
      end if;

   end Arrange_Non_Terminal;

   ----------------------
   -- Arrange_Terminal --
   ----------------------

   procedure Arrange_Terminal (Item    : in     Program_Tree;
                               Context : in out Arrangement_Context)
   is
      Format    : constant Aquarius_Format := Item.Syntax.Get_Format;
      Rules     : constant Immediate_Rules := Formats.Rules (Format);
   begin

      if Item.Vertical_Gap_Before > 0 then
         Context.Current_Line :=
           Context.Current_Line + Item.Vertical_Gap_Before;
         Context.Current_Column := 1;
      end if;

      if Enabled (Rules.New_Line_Before) then
         Context.Need_New_Line := True;
      end if;

      if Context.Need_New_Line then
         if not Context.Got_New_Line then
            Context.Current_Line     := Context.Current_Line + 1;
            Context.Current_Column   := 1;
            Context.Need_New_Line := False;
            Context.Got_New_Line  := True;
            Context.Need_Space    := False;
            Context.First_On_Line := True;
         end if;
      end if;

      if Context.First_On_Line then
         if Context.Cancel_Indent then
            Context.Cancel_Indent := False;
         else
            Context.Current_Column   := Context.Current_Indent;
         end if;
      else
         declare
            Insert_Space : Boolean;
         begin
            if not Formats.Enabled (Rules.Space_Before) then
               Insert_Space := Context.Need_Space;
            elsif not Formats.Negative (Rules.Space_Before) then
               Insert_Space := not Context.No_Space or else
                 Context.Space_Priority < Priority (Rules.Space_Before);
            else
               if Context.Need_Space then
                  Insert_Space :=
                    Context.Space_Priority >= Priority (Rules.Space_Before);
               else
                  Insert_Space := False;
               end if;
            end if;

            if Insert_Space then
               Context.Current_Column := Context.Current_Column + 1;
            end if;
         end;
      end if;

      Item.Start_Position := (Context.Current_Line,
                              Context.Current_Column);
      Item.End_Position := (Context.Current_Line,
                            Context.Current_Column + Item.Layout_Length);

      if Item.Program_Left = null then
         declare
            It : Program_Tree := Item.Program_Parent;
         begin
            while It /= null and then It.Program_Left = null loop
               It.Start_Position := Item.Start_Position;
               It := It.Program_Parent;
            end loop;
         end;
      end if;

      if Item.Program_Right = null then
         declare
            It : Program_Tree := Item.Program_Parent;
         begin
            while It /= null and then It.Program_Right = null loop
               It.End_Position := Item.Start_Position;
               It := It.Program_Parent;
            end loop;
         end;
      end if;

      if Context.User_Text_Length > 0 and then
        Before_Point (Item, Context.User_Cursor)
      then
         Context.Current_Column :=
           Context.Current_Column + Context.User_Text_Length;
      end if;

      if Item.Separator_New_Line or else
        (Enabled (Rules.New_Line_After) and then Item.Is_Separator)
      then

         Context.Current_Line := Context.Current_Line + 1;
         declare
            Align_With : constant Program_Tree :=
              Program_Tree (Item.Parent.First_Leaf);
         begin
            Context.Current_Column := Align_With.Layout_Start_Column;
         end;
         Context.Got_New_Line  := True;
         Context.Need_New_Line := False;
         Context.First_On_Line := True;
         Context.Cancel_Indent := True;
      else
         Context.Current_Column :=
           Context.Current_Column + Item.Layout_Length;
         Context.Current_Position :=
           Context.Current_Position + Item.Layout_Length;

         if Positive (Context.Current_Column) > Context.Right_Margin and then
           not Context.First_On_Line and then
           not Item.Overflow_Checked
         then
            if not Context.Overflow then
               Context.Overflow := True;
               Context.Overflow_Line := Context.Current_Line;
               Item.Overflow_Checked := True;
            end if;
         else
            Context.Overflow := False;
         end if;

         Context.First_On_Line := False;
         Context.Got_New_Line  := False;

         if Enabled (Rules.Space_After) then
            if Negative (Rules.Space_After) then
               Context.No_Space := True;
               Context.Need_Space := False;
            else
               Context.Need_Space := True;
               Context.No_Space := False;
            end if;
            Context.Space_Priority := Priority (Rules.Space_After);
         end if;

         if Enabled (Rules.New_Line_After) then
            Context.Need_New_Line := True;
         end if;
      end if;

   end Arrange_Terminal;

   ------------------
   -- Before_Point --
   ------------------

   function Before_Point (Location : Program_Tree;
                          Point    : Aquarius.Trees.Cursors.Cursor)
                         return Boolean
   is
      use Aquarius.Trees.Cursors;
   begin
      return not Is_Off_Left (Point) and then
        Location = Program_Tree (Get_Left_Tree (Point));
   end Before_Point;

   ------------
   -- Render --
   ------------

   procedure Render
     (Program     : in Program_Tree;
      Renderer    : in Aquarius.Rendering.Aquarius_Renderer;
      Point       : in Aquarius.Trees.Cursors.Cursor;
      Partial     : in String)
   is

      Point_Position : Aquarius.Layout.Position :=
        (1, 1 + Aquarius.Layout.Count (Partial'Length));

      procedure Perform_Render (P : Program_Tree);
      procedure Render_Terminal (Terminal : Program_Tree);

      --------------------
      -- Perform_Render --
      --------------------

      procedure Perform_Render (P : Program_Tree) is
      begin
         if P.Is_Terminal then
            Render_Terminal (P);
         else
            for I in 1 .. P.Child_Count loop
               Perform_Render (P.Program_Child (I));
            end loop;
         end if;
      end Perform_Render;

      ---------------------
      -- Render_Terminal --
      ---------------------

      procedure Render_Terminal (Terminal : Program_Tree) is
         use Aquarius.Messages;
         Render_Position : constant Aquarius.Layout.Position :=
           Terminal.Layout_Start_Position;
         Msg_Level       : constant Message_Level :=
           Terminal.Get_Inherited_Message_Level;
      begin
         if Msg_Level > No_Message then
            Renderer.Set_Text (Render_Position,
                               Level_Name (Msg_Level),
                               Terminal.Text);
         elsif not Aquarius.Syntax.Is_Empty (Terminal.Render_Class) then
            Renderer.Set_Text (Render_Position,
                               Terminal.Render_Class.Render_Class,
                               Terminal.Text);
         elsif Terminal.Syntax.Has_Token then
            Renderer.Set_Text (Render_Position,
                               Aquarius.Tokens.Get_Token_Class_Name
                                 (Terminal.Syntax.Frame,
                                  Terminal.Syntax.Token),
                               Terminal.Text);
         else
            Renderer.Set_Text (Render_Position, "normal", Terminal.Text);
         end if;

         if Before_Point (Terminal, Point) then
            Point_Position := Terminal.Layout_End_Position;
            Renderer.Set_Text (Point_Position, "normal", Partial);
            Point_Position.Column :=
              Point_Position.Column + Partial'Length;
         end if;

      end Render_Terminal;

   begin
      Renderer.Begin_Render;
      Perform_Render (Program);
      Renderer.Set_Point (Point_Position);
      Renderer.End_Render;
   end Render;

end Aquarius.Programs.Arrangements;
