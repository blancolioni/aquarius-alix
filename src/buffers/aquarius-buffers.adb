with Ada.Directories;
with Ada.Strings.Fixed;

with Aquarius.Actions;
with Aquarius.Grammars.Manager;
with Aquarius.Loader;
with Aquarius.Programs.Arrangements;
with Aquarius.Projects;
with Aquarius.Rendering.Manager;
with Aquarius.Tokens;
with Aquarius.Trees.Cursors;
with Aquarius.Trees.Properties;

with Aquarius.Tasks.Actions;

package body Aquarius.Buffers is

   function New_Empty_Buffer
     (UI      : not null access Aquarius.UI.Aquarius_UI'Class;
      Grammar : Aquarius.Grammars.Aquarius_Grammar)
     return Aquarius_Buffer;

   function New_Program_Buffer
     (UI      : not null access Aquarius.UI.Aquarius_UI'Class;
      Grammar : Aquarius.Grammars.Aquarius_Grammar)
     return Aquarius_Buffer;

   procedure Scan_Token
     (Buffer      : not null access Aquarius_Buffer_Record'Class;
      Text        : in     String;
      Force       : in     Boolean;
      Length      :    out Natural;
      Tok         :    out Aquarius.Tokens.Token);

   procedure Insert_Character
     (Buffer : not null access Aquarius_Buffer_Record'Class;
      Char   : in     Character);

   function Check_Token
     (Buffer : not null access Aquarius_Buffer_Record'Class;
      Force  : in     Boolean)
     return Boolean;

   procedure Move_By_Terminal
     (Buffer  : not null access Aquarius_Buffer_Record'Class;
      Current : in out Aquarius.Trees.Cursors.Cursor;
      Forward : in     Boolean);

   procedure Set_Program
     (Buffer  : not null access Aquarius_Buffer_Record'Class;
      Program : not null access Aquarius.Programs.Program_Tree_Type'Class);

   --------------------
   -- Attach_Message --
   --------------------

   overriding
   procedure Attach_Message
     (To    : in out Aquarius_Buffer_Record;
      Item  : in     Aquarius.Messages.Message)
   is
   begin
      Aquarius.Messages.Add_Message (To.Local_Messages, Item);
   end Attach_Message;

   ------------
   -- Before --
   ------------

   overriding
   function Before
     (Left   : Aquarius_Buffer_Record;
      Right  : not null access Aquarius.Messages.Message_Location'Class)
      return Boolean
   is
   begin
      return Left.Show_Location < Right.Show_Location;
   end Before;

   -----------------
   -- Check_Token --
   -----------------

   function Check_Token
     (Buffer : not null access Aquarius_Buffer_Record'Class;
      Force  : in     Boolean)
      return Boolean
   is
      use Aquarius.Programs.Parser;
      Tok       : Aquarius.Tokens.Token;
      Length    : Natural;
      Got_Token : Boolean := False;
      Start     : constant Natural :=
        Ada.Strings.Fixed.Index_Non_Blank
        (Buffer.Input_Buffer (1 .. Buffer.Input_Length));
   begin
      if Start = 0 then
         Buffer.Input_Length := 0;
         return False;
      end if;

      while Buffer.Input_Length > 0 loop
         Scan_Token (Buffer,
                     Buffer.Input_Buffer (1 .. Buffer.Input_Length),
                     Force, Length, Tok);
         exit when Length = 0;
         Got_Token := True;
         Aquarius.Source.Set_Position
           (Buffer.File_Position,
            Aquarius.Source.Line_Number (Buffer.Point_Position.Line),
            Aquarius.Source.Column_Number (Buffer.Point_Position.Column));

         declare
            use type Aquarius.Tokens.Token;
            Text : constant String := Buffer.Input_Buffer (Start .. Length);
         begin
            if Buffer.Changing and then
              Tok = Buffer.Editing_Node.Get_Token
            then
               Buffer.Editing_Node.Fill (Text);
            elsif Token_OK (Tok, Buffer.File_Position, Buffer.Parsing) then
               Parse_Token (Tok, Buffer.File_Position, Text, Buffer.Parsing);
            else
               Buffer.Input_Length := Buffer.Input_Length - 1;
               Buffer.Input_Position := Buffer.Input_Position - 1;
               exit;
            end if;
            Buffer.Input_Buffer (1 .. Buffer.Input_Length - Length) :=
              Buffer.Input_Buffer (Length + 1 .. Buffer.Input_Length);
            Buffer.Input_Length := Buffer.Input_Length - Length;
            Buffer.Input_Position := Buffer.Input_Position - Length;
         end;
      end loop;

      if Got_Token and then
        not Aquarius.Programs.Parser.Is_Ambiguous (Buffer.Parsing)
      then
         Aquarius.Programs.Arrangements.Arrange (Buffer.Contents);
         Render (Buffer);
         Buffer.Buffer_UI.Update_Message_View;
         return True;
      else
         return False;
      end if;

   end Check_Token;

   --------------------
   -- Clear_Messages --
   --------------------

   overriding
   procedure Clear_Messages
     (Item : in out Aquarius_Buffer_Record)
   is
   begin
      Aquarius.Messages.Clear_Message_List (Item.Local_Messages);
   end Clear_Messages;

   ----------------------
   -- File_Simple_Name --
   ----------------------

   function File_Simple_Name
     (Buffer : not null access Aquarius_Buffer_Record'Class)
     return String
   is
   begin
      return Ada.Directories.Simple_Name
        (Ada.Strings.Unbounded.To_String (Buffer.Full_Path));
   end File_Simple_Name;

   ---------------
   -- Full_Path --
   ---------------

   function Full_Path
     (Buffer : not null access Aquarius_Buffer_Record'Class)
     return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Buffer.Full_Path);
   end Full_Path;

   ------------------
   -- Get_Messages --
   ------------------

   overriding
   procedure Get_Messages (From  : Aquarius_Buffer_Record;
                           List  : in out Aquarius.Messages.Message_List)
   is
      use type Aquarius.Programs.Program_Tree;
   begin
      if From.Contents /= null then
         From.Contents.Get_Messages (List);
      end if;
      --  Aquarius.Messages.Copy_Message_List (From.Local_Messages, List);
   end Get_Messages;

   -------------
   -- Grammar --
   -------------

   function Grammar (Buffer : not null access Aquarius_Buffer_Record'Class)
                     return Aquarius.Grammars.Aquarius_Grammar
   is
   begin
      return Buffer.Grammar;
   end Grammar;

   ----------------------
   -- Insert_Character --
   ----------------------

   procedure Insert_Character
     (Buffer : not null access Aquarius_Buffer_Record'Class;
      Char   : in     Character)
   is
      use type Aquarius.Layout.Count;
   begin
      if not Buffer.Typing then
         Buffer.Typing     := True;
         Buffer.Input_Length := 0;
         Buffer.Input_Position := 1;
      end if;
      Buffer.Input_Buffer
        (Buffer.Input_Position + 1 .. Buffer.Input_Length + 1) :=
        Buffer.Input_Buffer (Buffer.Input_Position .. Buffer.Input_Length);
      Buffer.Input_Length := Buffer.Input_Length + 1;
      Buffer.Input_Buffer (Buffer.Input_Position) := Char;
      Buffer.Input_Position := Buffer.Input_Position + 1;
      Buffer.Point_Position.Column :=
        Buffer.Point_Position.Column + 1;
   end Insert_Character;

   ----------
   -- Load --
   ----------

   procedure Load (Buffer : in out Aquarius_Buffer_Record'Class) is
      use Ada.Strings.Unbounded;
   begin
      Buffer.Contents :=
        Aquarius.Loader.Load_From_File (Buffer.Grammar,
                                        Buffer.Program_Store,
                                        Buffer'Access,
                                        Buffer.Buffer_UI,
                                        To_String (Buffer.Full_Path));
      declare
         Action : Aquarius.Tasks.Actions.Action_Task_Type;
      begin
         Action.Create (Buffer.Grammar,
                        Aquarius.Actions.Semantic_Trigger,
                        Buffer.Contents);
         Action.Add_Task;
      end;

   end Load;

   ---------------------------
   -- Load_Buffer_From_File --
   ---------------------------

   function Load_Buffer_From_File
     (UI      : not null access Aquarius.UI.Aquarius_UI'Class;
      Path    : String)
     return Aquarius_Buffer
   is
      Result : constant Aquarius_Buffer :=
        New_Buffer_From_File (UI, Path);
   begin
      Result.Load;
      return Result;
   end Load_Buffer_From_File;

   ---------------------
   -- Location_Column --
   ---------------------

   overriding
   function Location_Column (Location : Aquarius_Buffer_Record)
                            return Positive
   is
   begin
      return Positive (Location.Point_Position.Column);
   end Location_Column;

   -------------------
   -- Location_Line --
   -------------------

   overriding
   function Location_Line (Location : Aquarius_Buffer_Record)
                          return Positive
   is
   begin
      return Positive (Location.Point_Position.Line);
   end Location_Line;

   -------------------
   -- Location_Name --
   -------------------

   overriding
   function Location_Name
     (Location : Aquarius_Buffer_Record)
     return String
   is
   begin
      return Location.Name;
   end Location_Name;

   ----------------------
   -- Move_By_Terminal --
   ----------------------

   procedure Move_By_Terminal
     (Buffer  : not null access Aquarius_Buffer_Record'Class;
      Current : in out Aquarius.Trees.Cursors.Cursor;
      Forward : in     Boolean)
   is
      use type Aquarius.Trees.Tree;
      use Aquarius.Trees.Cursors;
      use Aquarius.Programs;
      It : Aquarius.Trees.Tree;

      function Is_Filled_Terminal
        (T : not null access constant Aquarius.Trees.Root_Tree_Type'Class)
        return Boolean;

      ------------------------
      -- Is_Filled_Terminal --
      ------------------------

      function Is_Filled_Terminal
        (T : not null access constant Aquarius.Trees.Root_Tree_Type'Class)
        return Boolean
      is
         P : constant Program_Tree_Type'Class :=
               Program_Tree_Type'Class (T.all);
      begin
         return P.Is_Terminal and then P.Text /= "";
      end Is_Filled_Terminal;

   begin
      if Forward then

         if Is_Off_Right (Current) then
            It := Get_Left_Tree (Current).Next_Leaf;
         else
            It := Get_Right_Tree (Current);
         end if;

         if It /= null then

            It := It.First_Leaf;

         end if;

         if It /= null then

            It := It.Search_Leaves (Aquarius.Trees.Forewards,
                                    Is_Filled_Terminal'Access);
         end if;

         if It /= null then
            Current := Right_Of_Tree (It);
            Buffer.Current_Render.Set_Point
              (Program_Tree (It).Layout_End_Position);
            Aquarius.Programs.Parser.Set_Cursor (Buffer.Parsing,
                                                 Current);
         end if;
      else
         if Is_Off_Left (Current) then
            It := Get_Right_Tree (Current).Previous_Leaf;
         else
            It := Get_Left_Tree (Current);
         end if;

         if It /= null then

            It := It.Previous_Leaf;

         end if;

         if It /= null then
            It := It.Search_Leaves (Aquarius.Trees.Backwards,
                                    Is_Filled_Terminal'Access);
         end if;

         if It /= null then
            Current := Left_Of_Tree (It);
            Buffer.Current_Render.Set_Point
              (Program_Tree (It).Layout_End_Position);
            Aquarius.Programs.Parser.Set_Cursor (Buffer.Parsing,
                                                 Current);
         end if;
      end if;
   end Move_By_Terminal;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Buffer : Aquarius_Buffer_Record)
                 return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Buffer.Buffer_Name);
   end Name;

   --------------------------
   -- New_Buffer_From_File --
   --------------------------

   function New_Buffer_From_File
     (UI   : not null access Aquarius.UI.Aquarius_UI'Class;
      Path : String)
     return Aquarius_Buffer
   is
   begin
      return New_Buffer_From_File
        (UI, Path,
         Aquarius.Projects.New_Default_Project (Path));
   end New_Buffer_From_File;

   --------------------------
   -- New_Buffer_From_File --
   --------------------------

   function New_Buffer_From_File
     (UI      : not null access Aquarius.UI.Aquarius_UI'Class;
      Path    : String;
      Store   : not null access Programs.Root_Program_Tree_Store'Class)
     return Aquarius_Buffer
   is
      use type Aquarius.Grammars.Aquarius_Grammar;
      Result : Aquarius_Buffer;
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Manager.Get_Grammar_For_File (Path);

   begin
      if Grammar /= null then
         Result := New_Program_Buffer (UI, Grammar);
         Result.Program_Store := Programs.Program_Tree_Store (Store);
         Result.File_Buffer := True;
         Result.Buffer_Name :=
           Ada.Strings.Unbounded.To_Unbounded_String
           (Ada.Directories.Simple_Name (Path));
         Result.Full_Path   :=
           Ada.Strings.Unbounded.To_Unbounded_String
           (Ada.Directories.Full_Name (Path));
      end if;
      return Result;
   end New_Buffer_From_File;

   --------------------------
   -- New_Buffer_From_Tree --
   --------------------------

   function New_Buffer_From_Tree
     (UI      : not null access Aquarius.UI.Aquarius_UI'Class;
      Name    : String;
      Program : not null access Programs.Program_Tree_Type'Class)
      return Aquarius_Buffer
   is
      Result : constant Aquarius_Buffer :=
                 New_Empty_Buffer
                   (UI, Aquarius.Trees.Properties.Get_Grammar (Program.all));
   begin
      Result.Buffer_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Set_Program (Result, Program);
      return Result;
   end New_Buffer_From_Tree;

   ----------------------
   -- New_Empty_Buffer --
   ----------------------

   function New_Empty_Buffer
     (UI           : not null access Aquarius.UI.Aquarius_UI'Class;
      Grammar_Name : String)
     return Aquarius_Buffer
   is
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Manager.Get_Grammar (Grammar_Name);
   begin
      return New_Empty_Buffer (UI, Grammar);
   end New_Empty_Buffer;

   ----------------------
   -- New_Empty_Buffer --
   ----------------------

   function New_Empty_Buffer
     (UI      : not null access Aquarius.UI.Aquarius_UI'Class;
      Grammar : Aquarius.Grammars.Aquarius_Grammar)
     return Aquarius_Buffer
   is
      Result : constant Aquarius_Buffer := new Aquarius_Buffer_Record;
   begin
      Aquarius.Messages.Create_Message_List (Result.Local_Messages, True);

      Result.Buffer_UI   := UI;
      Result.File_Buffer := False;
      Result.Grammar     := Grammar;
      Result.Buffer_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String ("Untitled");
      Result.Current_Render := null;
      return Result;
   end New_Empty_Buffer;

   ------------------------
   -- New_Program_Buffer --
   ------------------------

   function New_Program_Buffer
     (UI      : not null access Aquarius.UI.Aquarius_UI'Class;
      Grammar : Aquarius.Grammars.Aquarius_Grammar)
     return Aquarius_Buffer
   is
      Result : constant Aquarius_Buffer :=
                 New_Empty_Buffer (UI, Grammar);
   begin
      Set_Program (Result,
                   Aquarius.Programs.New_Program_Tree
                     (Grammar.Get_Top_Level_Syntax));
      return Result;
   end New_Program_Buffer;

   ------------
   -- On_Key --
   ------------

   function On_Key (Buffer : not null access Aquarius_Buffer_Record'Class;
                    Key    : in     Aquarius.Keys.Aquarius_Key)
                   return Boolean
   is
      pragma Unreferenced (Buffer);
      pragma Unreferenced (Key);
   begin
      return False;
   end On_Key;

   --     use Aquarius.Keys;
   --  begin
   --     if Is_Character (Key) then
   --        Buffer.Insert_Character (Get_Character (Key));
   --        return Check_Token (Buffer, False);
   --     elsif Raw_Key (Key) = Back_Space then
   --        if Buffer.Input_Length > 0 then
   --           Buffer.Input_Length := Buffer.Input_Length - 1;
   --           return False;
   --        else
   --           return True;
   --        end if;
   --     else
   --        declare
   --           Handled : constant Boolean := Check_Token (Buffer, True);
   --        begin
   --           if Raw_Key (Key) = Tab or else Raw_Key (Key) = Left_Tab then
   --              Move_By_Terminal (Buffer, Buffer.Point_Cursor,
   --                                Raw_Key (Key) = Tab);
   --              return True;
   --           else
   --              return Handled;
   --           end if;
   --        end;
   --     end if;
   --  end On_Key;

   ------------
   -- On_Key --
   ------------

   function On_Key (Buffer : not null access Aquarius_Buffer_Record'Class;
                    Pos    : in     Aquarius.Layout.Position;
                    Key    : in     Aquarius.Keys.Aquarius_Key)
                   return Boolean
   is
      use type Aquarius.Keys.Aquarius_Key;
      use Aquarius.Layout;
      use Aquarius.Programs;
      Left_Terminal : constant Program_Tree :=
        Buffer.Contents.Find_Node_At (Pos);
   begin
      if Buffer.Typing and then
        Aquarius.Keys.Is_Character (Key) and then
        Pos = Buffer.Point_Position
      then
         Insert_Character (Buffer,
                           Aquarius.Keys.Get_Character (Key));
         return Check_Token (Buffer, False);
      end if;

      declare
         Cursor : Aquarius.Trees.Cursors.Cursor;
      begin
         if Left_Terminal /= null then
            Cursor :=
              Aquarius.Trees.Cursors.Right_Of_Tree (Left_Terminal);
         else
            Cursor :=
              Aquarius.Trees.Cursors.Left_Of_Tree (Buffer.Contents);
         end if;
         Aquarius.Programs.Parser.Set_Cursor (Buffer.Parsing,
                                              Cursor);
      end;

      Buffer.Input_Length := 0;
      Buffer.Point_Position := Pos;

      if Aquarius.Keys.Is_Character (Key) then
         Insert_Character (Buffer,
                           Aquarius.Keys.Get_Character (Key));
         return Check_Token (Buffer, False);
      elsif Aquarius.Keys.Raw_Key (Key) = Aquarius.Keys.Tab then
         declare
            Cursor : Aquarius.Trees.Cursors.Cursor :=
              Aquarius.Programs.Parser.Get_Cursor (Buffer.Parsing);
         begin
            Buffer.Move_By_Terminal
              (Cursor,
               Forward => not Aquarius.Keys.Shift (Key));
            return True;
         end;
      end if;

      return False;
   end On_Key;

   -------------
   -- Program --
   -------------

   function Program (Buffer : not null access Aquarius_Buffer_Record'Class)
                    return Aquarius.Programs.Program_Tree
   is
   begin
      return Buffer.Contents;
   end Program;

   ------------
   -- Render --
   ------------

   procedure Render
     (Buffer  : not null access Aquarius_Buffer_Record)
   is
      use type Aquarius.Rendering.Aquarius_Renderer;
   begin
      if Buffer.Current_Render = null then
         Buffer.Current_Render :=
           Aquarius.Rendering.Manager.Load_Renderer ("text");
      end if;
      Buffer.Render (Buffer.Current_Render);
   end Render;

   ------------
   -- Render --
   ------------

   procedure Render
     (Buffer  : not null access Aquarius_Buffer_Record;
      Display : not null access
      Aquarius.Rendering.Root_Aquarius_Renderer'Class)
   is
      Cursor : constant Aquarius.Trees.Cursors.Cursor :=
        Aquarius.Programs.Parser.Get_Cursor
        (Buffer.Parsing);
   begin
      Aquarius.Programs.Arrangements.Arrange (Buffer.Contents,
                                              Cursor,
                                              Buffer.Input_Length);
      Aquarius.Programs.Arrangements.Render
        (Buffer.Contents,
         Aquarius.Rendering.Aquarius_Renderer (Display),
         Cursor,
         Buffer.Input_Buffer (1 .. Buffer.Input_Length));
   end Render;

   ----------------
   -- Scan_Token --
   ----------------

   procedure Scan_Token
     (Buffer      : not null access Aquarius_Buffer_Record'Class;
      Text        : in     String;
      Force       : in     Boolean;
      Length      :    out Natural;
      Tok         :    out Aquarius.Tokens.Token)
   is
      Complete    : Boolean;
      Have_Class  : Boolean;
      Class       : Aquarius.Tokens.Token_Class;
      First, Next : Natural := 1;
   begin
      Aquarius.Tokens.Scan (Buffer.Grammar.Frame, Text,
                            False, Complete, Have_Class,
                            Class, Tok, First, Next);
      if (Force and then Next > 0) or else
        (Have_Class and then Complete and then Next < Text'Length)
      then
         Length := Next;
      else
         Length := 0;
      end if;

   end Scan_Token;

   ------------------------
   -- Set_Current_Render --
   ------------------------

   procedure Set_Current_Render
     (Buffer  : not null access Aquarius_Buffer_Record;
      Display : not null access
      Aquarius.Rendering.Root_Aquarius_Renderer'Class)
   is
   begin
      Buffer.Current_Render :=
        Aquarius.Rendering.Aquarius_Renderer (Display);
   end Set_Current_Render;

   ---------------
   -- Set_Point --
   ---------------

   procedure Set_Point (Buffer : not null access Aquarius_Buffer_Record'Class;
                        Point  : in     Aquarius.Layout.Position)
   is
      use Aquarius.Layout;
      Right_Tree : Aquarius.Programs.Program_Tree;
   begin

      Right_Tree := Buffer.Contents.Find_Node_At (Point);
      if Point = Buffer.Point_Position then
         return;
      end if;

      Aquarius.Programs.Parser.Set_Cursor
        (Buffer.Parsing,
         Aquarius.Trees.Cursors.Left_Of_Tree (Right_Tree));
      Buffer.Node_Offset  := Point.Column - Right_Tree.Layout_Start_Column;
      --  FIXME: if anything has already been typed, parse it and
      --  insert it or create error trees
      declare
         Text : constant String := Right_Tree.Text;
      begin
         Buffer.Input_Length := Text'Length;
         Buffer.Input_Buffer (1 .. Text'Length) := Text;
         Buffer.Input_Position :=
           Positive (Point.Column -
                       Right_Tree.Layout_Start_Position.Column + 1);
         Buffer.Changing     := True;
         Buffer.Typing       := True;
         Buffer.Editing_Node := Right_Tree;
      end;
      Buffer.Point_Position := Point;
      Buffer.Current_Render.Set_Point (Point);
   end Set_Point;

   -----------------
   -- Set_Program --
   -----------------

   procedure Set_Program
     (Buffer  : not null access Aquarius_Buffer_Record'Class;
      Program : not null access Aquarius.Programs.Program_Tree_Type'Class)
   is
   begin
      Buffer.Contents :=
        Aquarius.Programs.Program_Tree (Program);
      Aquarius.Trees.Properties.Set_Grammar (Buffer.Contents.all,
                                             Buffer.Grammar);
      Buffer.Point_Position := (1, 1);
      Aquarius.Programs.Parser.Initialise_Parse_Context
        (Context      => Buffer.Parsing,
         Grammar      => Buffer.Grammar,
         Root         => Buffer.Contents,
         Interactive  => True);
      Buffer.Node_Offset := 0;

      --  Buffer.Buffer_File :=
      --    Aquarius.Source.Buffers.Buffer_File (Result);
      --  Buffer.File_Position :=
      --      Aquarius.Source.Get_Start (Buffer.Buffer_File);
      --  Aquarius.Source.Set_Position (Buffer.File_Position, 1, 1);
   end Set_Program;

   -------------------
   -- Show_Location --
   -------------------

   function Show_Location
     (Location : Aquarius_Buffer_Record)
     return String
   is
   begin
      return Location.Name;
   end Show_Location;

   ------------
   -- Update --
   ------------

   overriding
   procedure Update
     (Item  : in out  Aquarius_Buffer_Record;
      Start : not null access Aquarius.Trees.Root_Tree_Type'Class)
   is
      pragma Unreferenced (Start);
   begin
      Item.Render;
   end Update;

end Aquarius.Buffers;
