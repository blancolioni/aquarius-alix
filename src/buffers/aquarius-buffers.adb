with Ada.Directories;
with Ada.Text_IO;

with Aquarius.Actions;
with Aquarius.Grammars.Manager;
with Aquarius.Loader;
with Aquarius.Programs.Arrangements;
with Aquarius.Projects;
with Aquarius.Rendering.Manager;
with Aquarius.Trees.Properties;

--  with Aquarius.Tasks.Actions;

package body Aquarius.Buffers is

   function New_Empty_Buffer
     (UI   : Aquarius.UI.Aquarius_UI;
      Grammar : Aquarius.Grammars.Aquarius_Grammar)
     return Aquarius_Buffer;

   function New_Program_Buffer
     (UI   : Aquarius.UI.Aquarius_UI;
      Grammar : Aquarius.Grammars.Aquarius_Grammar)
     return Aquarius_Buffer;

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
      return Left.Name < Aquarius_Buffer (Right).Name;
   end Before;

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

   ----------
   -- Load --
   ----------

   procedure Load
     (Buffer      : in out Aquarius_Buffer_Record'Class;
      Synchronous : Boolean)
   is
      use Ada.Strings.Unbounded;
   begin
      Buffer.Contents :=
        Aquarius.Loader.Load_From_File (Buffer.Grammar,
                                        Buffer.Program_Store,
                                        Buffer'Access,
                                        Buffer.Buffer_UI,
                                        To_String (Buffer.Full_Path));

      if True or else Synchronous then
         Buffer.Grammar.Run_Action_Trigger
           (Start      => Buffer.Contents,
            Trigger    => Aquarius.Actions.Semantic_Trigger);
      else
         null;
--           declare
--              Action : Aquarius.Tasks.Actions.Action_Task_Type;
--           begin
--              Action.Create (Buffer.Grammar,
--                             Aquarius.Actions.Semantic_Trigger,
--                             Buffer.Contents);
--              Action.Add_Task;
--           end;
      end if;

   end Load;

   ---------------------------
   -- Load_Buffer_From_File --
   ---------------------------

   function Load_Buffer_From_File
     (UI   : Aquarius.UI.Aquarius_UI;
      Path    : String)
     return Aquarius_Buffer
   is
      Result : constant Aquarius_Buffer :=
        New_Buffer_From_File (UI, Path);
   begin
      Result.Load (True);
      return Result;
   end Load_Buffer_From_File;

   ---------------------
   -- Location_Column --
   ---------------------

   overriding
   function Location_Column (Location : Aquarius_Buffer_Record)
                            return Natural
   is
   begin
      return Positive (Location.Point_Position.Column);
   end Location_Column;

   -------------------
   -- Location_Line --
   -------------------

   overriding
   function Location_Line (Location : Aquarius_Buffer_Record)
                          return Natural
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
     (UI   : Aquarius.UI.Aquarius_UI;
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
     (UI   : Aquarius.UI.Aquarius_UI;
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
     (UI   : Aquarius.UI.Aquarius_UI;
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
     (UI   : Aquarius.UI.Aquarius_UI;
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
     (UI   : Aquarius.UI.Aquarius_UI;
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
     (UI   : Aquarius.UI.Aquarius_UI;
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
      use Aquarius.Keys;
      Handled : Boolean := True;
   begin
      if Is_Character (Key) then
         declare
            Echo : Boolean;
         begin
            Buffer.Editor.Insert_Character (Get_Character (Key), Echo);
            Handled := not Echo;
         end;
      elsif Raw_Key (Key) = Back_Space then
         Buffer.Editor.Backspace;
         Handled := False;
      else
         Handled := False;
      end if;
      return Handled;
   end On_Key;

   ------------
   -- On_Key --
   ------------

--     function On_Key (Buffer : not null access Aquarius_Buffer_Record'Class;
--                      Pos    : in     Aquarius.Layout.Position;
--                      Key    : in     Aquarius.Keys.Aquarius_Key)
--                     return Boolean
--     is
--     begin
--        if Aquarius.Keys.Is_Character (Key) then
--           Buffer.Editor.Insert_Character
--
--        use type Aquarius.Keys.Aquarius_Key;
--        use Aquarius.Layout;
--        use Aquarius.Programs;
--        Left_Terminal : constant Program_Tree :=
--          Buffer.Contents.Find_Node_At (Pos);
--     begin
--        if Buffer.Typing and then
--          Aquarius.Keys.Is_Character (Key) and then
--          Pos = Buffer.Point_Position
--        then
--           Buffer.Changing := True;
--           Insert_Character (Buffer,
--                             Aquarius.Keys.Get_Character (Key));
--           return Check_Token (Buffer, False);
--        end if;
--
--        declare
--           Cursor : Aquarius.Trees.Cursors.Cursor;
--        begin
--           if Left_Terminal /= null then
--
--              if Left_Terminal.Contains_Position (Pos)
--                and then Aquarius.Keys.Is_Character (Key)
--              then
--                 declare
--                    Text : constant String := Left_Terminal.Text;
--                 begin
--                    Buffer.Input_Length := Text'Length;
--                    Buffer.Input_Buffer (1 .. Buffer.Input_Length) :=
--                      Text;
--                    Buffer.Input_Position :=
--                 Natural (Pos.Column - Left_Terminal.Layout_Start_Column)
--                      + 1;
--                    Buffer.Typing := True;
--                    Buffer.Changing := True;
--                    Buffer.Editing_Node := Left_Terminal;
--                    Ada.Text_IO.Put_Line
--                      ("Buffer: "
--                       & Buffer.Input_Buffer (1 .. Buffer.Input_Length));
--                 end;
--              end if;
--
--              Cursor :=
--                Aquarius.Trees.Cursors.Right_Of_Tree (Left_Terminal);
--           else
--              Cursor :=
--                Aquarius.Trees.Cursors.Left_Of_Tree (Buffer.Contents);
--              Buffer.Input_Length := 0;
--           end if;
--
--           Aquarius.Programs.Parser.Set_Cursor (Buffer.Parsing,
--                                                Cursor);
--           Ada.Text_IO.Put_Line ("Cursor: "
--                                 & Aquarius.Trees.Cursors.Image (Cursor));
--        end;
--
--        Buffer.Point_Position := Pos;
--
--        if Aquarius.Keys.Is_Character (Key) then
--           Insert_Character (Buffer,
--                             Aquarius.Keys.Get_Character (Key));
--           return Check_Token (Buffer, False);
--        elsif Aquarius.Keys.Raw_Key (Key) = Aquarius.Keys.Tab then
--           declare
--              Cursor : Aquarius.Trees.Cursors.Cursor :=
--                Aquarius.Programs.Parser.Get_Cursor (Buffer.Parsing);
--           begin
--              Buffer.Move_By_Terminal
--                (Cursor,
--                 Forward => not Aquarius.Keys.Shift (Key));
--              return True;
--           end;
--        elsif Aquarius.Keys.Raw_Key (Key) = Aquarius.Keys.Back_Space then
--           if Buffer.Input_Length > 0 then
--              Buffer.Input_Length := Buffer.Input_Length - 1;
--              return False;
--           else
--              return True;
--           end if;
--        end if;
--
--        return False;
--     end On_Key;

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

      Ada.Text_IO.Put_Line ("Start render: " & Buffer.Name);

      Buffer.Rendering := True;

      Aquarius.Programs.Arrangements.Arrange (Buffer.Contents,
                                              Cursor,
                                              0);

      Aquarius.Programs.Arrangements.Render
        (Buffer.Contents,
         Aquarius.Rendering.Aquarius_Renderer (Display),
         Cursor,
         "");

      Buffer.Rendering := False;
      Ada.Text_IO.Put_Line ("Finish render: " & Buffer.Name);

   end Render;

   ------------------------
   -- Set_Current_Render --
   ------------------------

   procedure Set_Current_Render
     (Buffer  : not null access Aquarius_Buffer_Record;
      Display : not null access
      Aquarius.Rendering.Root_Aquarius_Renderer'Class)
   is
      use type Aquarius.Programs.Editor.Program_Editor;
   begin
      Buffer.Current_Render :=
        Aquarius.Rendering.Aquarius_Renderer (Display);
      if Buffer.Editor = null then
         Buffer.Editor :=
           Aquarius.Programs.Editor.Create_Editor
             (Buffer.Contents);
         Buffer.Editor.Add_Renderer (Buffer, Buffer.Contents);
      end if;
   end Set_Current_Render;

   ---------------
   -- Set_Point --
   ---------------

   procedure Set_Point (Buffer : not null access Aquarius_Buffer_Record'Class;
                        Point  : in     Aquarius.Layout.Position)
   is
--        use Aquarius.Layout;
--        Right_Tree : constant Aquarius.Programs.Program_Tree :=
--                       Buffer.Contents.Find_Node_Containing (Point);
   begin

      Buffer.Editor.Set_Point (Point);

--        if Point = Buffer.Point_Position then
--           return;
--        end if;
--
--        if Buffer.Changing then
--           --  FIXME: if anything has already been typed, parse it and
--           --  insert it or create error trees
--           null;
--        end if;
--
--        Ada.Text_IO.Put_Line
--          ("Set_Point: " & Right_Tree.Image);
--
--        Aquarius.Programs.Parser.Set_Cursor
--          (Buffer.Parsing,
--           Aquarius.Trees.Cursors.Left_Of_Tree (Right_Tree));
--
--     Buffer.Node_Offset  := Point.Column - Right_Tree.Layout_Start_Column;
--
--        declare
--           Text : constant String := Right_Tree.Text;
--        begin
--           Buffer.Input_Length := Text'Length;
--           Buffer.Input_Buffer (1 .. Text'Length) := Text;
--           Buffer.Input_Position := Positive (Buffer.Node_Offset + 1);
--           Buffer.Typing       := True;
--           Buffer.Editing_Node := Right_Tree;
--        end;
--        Buffer.Point_Position := Point;
--        Buffer.Current_Render.Set_Point (Point);
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
--        Buffer.Editor :=
--          Aquarius.Programs.Editor.Create_Editor
--            (Buffer.Contents);

      --  Buffer.Buffer_File :=
      --    Aquarius.Source.Buffers.Buffer_File (Result);
      --  Buffer.File_Position :=
      --      Aquarius.Source.Get_Start (Buffer.Buffer_File);
      --  Aquarius.Source.Set_Position (Buffer.File_Position, 1, 1);
   end Set_Program;

   -------------------
   -- Show_Location --
   -------------------

--     function Show_Location
--       (Location : Aquarius_Buffer_Record)
--       return String
--     is
--     begin
--        return Location.Name;
--     end Show_Location;

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

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Buffer  : in out Aquarius_Buffer_Record;
      Point   : Aquarius.Trees.Cursors.Cursor;
      Partial : String)
   is
   begin
      Ada.Text_IO.Put_Line ("Start update: " & Buffer.Name);
      Ada.Text_IO.Put_Line
        ("  point: "
         & Aquarius.Trees.Cursors.Image (Point));

      Buffer.Rendering := True;

      Aquarius.Programs.Arrangements.Arrange
        (Item           => Buffer.Contents,
         Point          => Point,
         Partial_Length => Partial'Length);

      Aquarius.Programs.Arrangements.Render
        (Program  => Buffer.Contents,
         Renderer => Buffer.Current_Render,
         Point    => Point,
         Partial  => Partial);

      Buffer.Rendering := False;
      Ada.Text_IO.Put_Line ("Finish update: " & Buffer.Name);
   end Update;

end Aquarius.Buffers;
