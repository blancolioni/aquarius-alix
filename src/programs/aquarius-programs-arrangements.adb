--  with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with Aquarius.Formats;
with Aquarius.Layout;
with Aquarius.Messages;
with Aquarius.Messages.Files;

package body Aquarius.Programs.Arrangements is

   use Aquarius.Layout;
   use Aquarius.Formats;

   type Arrangement_Context is
      record
         Need_New_Line     : Boolean             := False;
         New_Line_Priority : Rule_Priority       := 1;
         Got_New_Line      : Boolean             := False;
         First_On_Line     : Boolean             := True;
         First_Terminal    : Program_Tree;
         Previous_Terminal : Program_Tree;
         Previous_Indent   : Count               := 1;
         Need_Space        : Boolean             := False;
         No_Space          : Boolean             := False;
         Cancel_Indent     : Boolean             := False;
         Message_Level     : Aquarius.Messages.Message_Level :=
           Aquarius.Messages.No_Message;
         Space_Priority    : Rule_Priority       := 1;
         Current_Line      : Count               := 1;
         Current_Column    : Count               := 1;
         Current_Position  : Count               := 0;
         Current_Indent    : Positive_Count      := 1;
         Right_Margin      : Positive_Count      := 72;
         Rearranging       : Boolean             := False;
         User_Text_Length  : Count               := 0;
         User_Cursor       : Aquarius.Trees.Cursors.Cursor;
         Logging           : Aquarius.Messages.Message_List;
         Stop_Tree         : Program_Tree        := null;
         Stopped           : Boolean             := False;
      end record;

   procedure Arrange
     (Item    : in     Program_Tree;
      Context : in out Arrangement_Context);

   procedure Arrange_Terminal
     (Item    : in     Program_Tree;
      Context : in out Arrangement_Context);

   procedure Arrange_Non_Terminal
     (Item    : in     Program_Tree;
      Context : in out Arrangement_Context);

   procedure Re_Arrange
     (Item    : in     Program_Tree;
      Context : in out Arrangement_Context;
      Start   :        Program_Tree;
      Finish  :        Program_Tree);

   function Before_Point (Location : Program_Tree;
                          Point    : Aquarius.Trees.Cursors.Cursor)
                         return Boolean;

   procedure Log
     (Context  : in out Arrangement_Context;
      Program  : Program_Tree;
      Text     : String);

   procedure Log
     (Context  : in out Arrangement_Context;
      Program  : Program_Tree);

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

      if Context.Stopped then
         return;
      end if;

      if Item.Name /= "" then
         Log (Context, Item);
      end if;

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

      if Context.Stop_Tree /= null and then Item = Context.Stop_Tree then
         Context.Stopped := True;
      end if;

   end Arrange;

   -------------
   -- Arrange --
   -------------

   procedure Arrange (Item        : in Program_Tree;
                      Line_Length : in Positive      := 72)
   is
      Context : Arrangement_Context;
   begin
      Context.Right_Margin := Positive_Count (Line_Length);
      Arrange (Item, Context);
      Aquarius.Messages.Files.Save_Messages
        ("arrangement.log", Context.Logging);
   exception
      when others =>
         Aquarius.Messages.Files.Save_Messages
           ("arrangement.log", Context.Logging);
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
      Context.Right_Margin := Positive_Count (Line_Length);
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
      Before_Indent : constant Indentation_Offset :=
                        Indent (Format, Before);
      After_Indent : constant Indentation_Offset :=
                        Indent (Format, After);
   begin

      if not Context.Rearranging
        and then Item.Vertical_Gap_Before > 0
      then
         Log (Context, Item, "vertical gap ="
              & Aquarius.Layout.Count'Image (Item.Vertical_Gap_Before));

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
               + Child_Indent + Before_Indent);

      Item.Start_Position := (Context.Current_Line,
                              Context.Current_Column);
      for I in 1 .. Item.Child_Count loop
         Arrange (Item.Program_Child (I), Context);
         if Context.Stopped then
            return;
         end if;
      end loop;

      Context.Current_Indent :=
        Count (Indentation_Offset (Context.Current_Indent)
               - Child_Indent + After_Indent);

      if Item.Soft_New_Line then
         Context.Current_Indent := Context.Current_Indent - 2;
      end if;

      if Enabled (Rules.New_Line_After) then
         Context.Need_New_Line := True;
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

      if not Context.Rearranging
        and then Item.Vertical_Gap_Before > 0
      then
         --  we already added all the vertical gaps when we first
         --  arranged this sub tree
         Log (Context, Item, "vertical gap before ="
              & Aquarius.Layout.Count'Image (Item.Vertical_Gap_Before));
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
         Log (Context, Item);
         if not Context.Rearranging
           and then Context.Previous_Terminal /= null
         then
            Log (Context, Context.First_Terminal, "previous line start");
            Log (Context, Context.Previous_Terminal, "previous line finish");
            if Context.Previous_Terminal.End_Position.Column
              > Context.Right_Margin
            then
               declare
                  Ancestor_Tree  : Aquarius.Trees.Tree;
                  Left_Ancestor  : Aquarius.Trees.Tree;
                  Right_Ancestor : Aquarius.Trees.Tree;
                  Ancestor       : Program_Tree;
               begin
                  Aquarius.Trees.Common_Ancestor
                    (Left           => Context.First_Terminal,
                     Right          => Context.Previous_Terminal,
                     Ancestor       => Ancestor_Tree,
                     Left_Ancestor  => Left_Ancestor,
                     Right_Ancestor => Right_Ancestor);
                  Ancestor := Program_Tree (Ancestor_Tree);
                  Log (Context, Ancestor, "re-arranging");
                  Re_Arrange (Ancestor, Context,
                              Context.First_Terminal,
                              Context.Previous_Terminal);
                  Log (Context, Ancestor, "after re-arrangement");
                  Log (Context, Ancestor);
               end;
            end if;

         end if;

         Context.First_Terminal := Item;
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

      if False then
         declare
            Position : Aquarius.Source.Source_Position :=
                         Item.Get_Location;
         begin
            Aquarius.Source.Set_Position
              (Position => Position,
               Line     =>
                 Aquarius.Source.Line_Number (Item.Start_Position.Line),
               Column   =>
                 Aquarius.Source.Column_Number (Item.Start_Position.Column));
            Item.Set_Location (Position);
         end;
      end if;

      if Item.Program_Left = null then
         declare
            It : Program_Tree := Item.Program_Parent;
         begin
            while It /= null and then It.Program_Left = null loop
               It.Start_Position := Item.Start_Position;
               --  It.Set_Location (Item.Get_Location);
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

      Context.Previous_Terminal := Item;
      Context.Previous_Indent := Context.Current_Indent;

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

   ---------
   -- Log --
   ---------

   procedure Log
     (Context  : in out Arrangement_Context;
      Program  : Aquarius.Programs.Program_Tree;
      Text     : String)
   is
      Current : constant String :=
                  (if Program.Name = ""
                   then "(" & Program.Parent.Name & ")"
                   else Program.Name
                   & (if Program.Name /= Program.Text
                     then "[" & Program.Text & "]"
                     else ""))
                   & ": ";

      Message : constant Aquarius.Messages.Message :=
                  Aquarius.Messages.New_Message
                    (Level    => Aquarius.Messages.Informational,
                     Location => Program,
                     Text     => Current & Text);
   begin
      Aquarius.Messages.Add_Message (Context.Logging, Message);
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log
     (Context  : in out Arrangement_Context;
      Program  : Program_Tree)
   is
--        use Ada.Characters.Handling;
      use Ada.Strings, Ada.Strings.Fixed;
--        function To_String (B : Boolean) return String
--        is (To_Lower (Boolean'Image (B)));
      function To_String (I : Integer) return String
      is (Trim (Integer'Image (I), Left));

--           Need_New_Line     : Boolean             := False;
--           New_Line_Priority : Rule_Priority       := 1;
--           Got_New_Line      : Boolean             := False;
--           First_On_Line     : Boolean             := True;
--           First_Terminal    : Program_Tree;
--           Previous_Terminal : Program_Tree;
--           Need_Space        : Boolean             := False;
--           No_Space          : Boolean             := False;
--           Cancel_Indent     : Boolean             := False;
--           Message_Level     : Aquarius.Messages.Message_Level :=
--             Aquarius.Messages.No_Message;
--           Space_Priority    : Rule_Priority       := 1;
--           Current_Line      : Count               := 1;
--           Current_Column    : Count               := 1;
--           Current_Position  : Count               := 0;
--           Current_Indent    : Count               := 1;
--           Right_Margin      : Positive_Count      := 72;
--           Overflow          : Boolean             := False;
--           Overflow_Line     : Count               := 0;
--           Rearranging       : Boolean             := False;
--           User_Text_Length  : Count               := 0;
--           User_Cursor       : Aquarius.Trees.Cursors.Cursor;
--           Logging           : Aquarius.Messages.Message_List;
   begin
      Log (Context, Program,
           (if Context.Need_New_Line then "[need-nl]" else "")
           & (if Context.Got_New_Line then "[got-nl]" else "")
           & (if Context.First_On_Line then "[first]" else "")
           & (if Context.Rearranging then "[rearranging]" else "")
          & ","
           & "line="
           & To_String (Integer (Context.Current_Line))
           & ","
           & "col="
           & To_String (Integer (Context.Current_Column))
           & ","
           & "indent="
           & To_String (Integer (Context.Current_Indent))
          );
   end Log;

   ----------------
   -- Re_Arrange --
   ----------------

   procedure Re_Arrange
     (Item    : in     Program_Tree;
      Context : in out Arrangement_Context;
      Start   : Program_Tree;
      Finish  : Program_Tree)
   is

      use Aquarius.Syntax;

      function Is_Separator
        (Item : Aquarius.Trees.Root_Tree_Type'Class)
         return Boolean
      is (Program_Tree_Type (Item).Is_Separator);

      Separator : constant Program_Tree :=
        Program_Tree
          (Item.Breadth_First_Search
             (Is_Separator'Access));

      Got_Start  : Boolean    := False;
      Got_Finish : Boolean    := False;

      Line_Length    : constant Positive_Count :=
                         Finish.End_Position.Column;
      Partial_Length   : Positive_Count := Context.Current_Indent;
      Remaining_Length : Count := Line_Length;
      New_Line_Indent  : constant Positive_Count :=
                           Context.Current_Indent + 2;
      Last_Column_Index : Positive_Count := Context.Current_Indent;

      Had_Soft_New_Line_After : Boolean := False;

      procedure Apply_Newlines
        (Program : Program_Tree);

      --------------------
      -- Apply_Newlines --
      --------------------

      procedure Apply_Newlines
        (Program : Program_Tree)
      is
      begin
         if Got_Finish then
            return;
         end if;

         if Program = Start then
            Got_Start := True;
            Last_Column_Index := Program.Start_Position.Column;
         elsif Program = Finish then
            Got_Finish := True;
         end if;

         if Got_Start then
            Remaining_Length := Line_Length - Program.Start_Position.Column;
            if Program.Is_Separator then
               if Separator /= null
                 and then Program.Syntax = Separator.Syntax
               then
                  Program.Separator_NL := True;
                  Partial_Length := New_Line_Indent;
               end if;
            elsif (Program.Has_Soft_New_Line_Rule_Before
                   or else Had_Soft_New_Line_After)
              and then Remaining_Length + Partial_Length
                > Context.Right_Margin
            then
--                 Log (Context, Program,
--                      "setting soft new line because remaining ="
--                      & Remaining_Length'Img
--                      & ", partial length ="
--                      & Partial_Length'Img
--                      & " and right margin ="
--                      & Context.Right_Margin'Img);
               Program.Set_Soft_New_Line;
               Partial_Length := New_Line_Indent;
            end if;

            Had_Soft_New_Line_After := False;

            if Program.Is_Terminal then
               Partial_Length := Partial_Length
                 + Program.End_Position.Column - Last_Column_Index;
               Last_Column_Index := Program.End_Position.Column;
            end if;

            if Program.Has_Soft_New_Line_Rule_After then
               Log (Context, Program, "soft new line follows this node");
               Had_Soft_New_Line_After := True;
            end if;

         end if;

         declare
            Children : constant Array_Of_Program_Trees :=
                         Program.Direct_Children
                           (Skip_Separators => False);
         begin
            for I in Children'Range loop
               Apply_Newlines (Children (I));
            end loop;
         end;
      end Apply_Newlines;

      Current_Indent : constant Count := Context.Current_Indent;

   begin

      Apply_Newlines (Item);

      Context.Current_Indent := Context.Previous_Indent;
      Context.Current_Line := Item.Start_Position.Line;
      Context.Current_Column := Item.Start_Position.Column;

      Context.Rearranging := True;
      Context.Stop_Tree := Finish;
      Context.Stopped   := False;
      Arrange (Item, Context);
      Context.Rearranging := False;
      Context.Stop_Tree := null;
      Context.Stopped := False;

      Context.Current_Line     := Context.Current_Line + 1;
      Context.Current_Column   := 1;
      Context.Need_New_Line := False;
      Context.Got_New_Line  := True;
      Context.Need_Space    := False;
      Context.First_On_Line := True;
      Context.Previous_Indent := Context.Current_Indent;
      Context.Current_Indent := Current_Indent;

   end Re_Arrange;

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
