with Ada.Strings.Fixed;
with Ada.Text_IO;

with Aquarius.Errors;
with Aquarius.Trees.Properties;

package body  Aquarius.Grammars is

   -------------------
   -- Action_Entity --
   -------------------

   function Action_Entity
     (Grammar     : Aquarius_Grammar_Record'Class;
      Group       : Aquarius.Actions.Action_Group;
      Position    : Aquarius.Actions.Action_Position;
      Parent_Name : String;
      Child_Name  : String)
      return Komnenos.Entities.Entity_Reference
   is
      Key : constant String :=
              Action_Entity_Key
                (Group_Name    =>
                   Aquarius.Actions.Action_Group_Name
                   (Group),
                 Position_Name =>
                   Aquarius.Actions.Show (Position),
                 Parent_Name   => Parent_Name,
                 Child_Name    => Child_Name);
   begin
      if Grammar.Action_Entities.Contains (Key) then
         return Grammar.Action_Entities.Element (Key);
      else
         return null;
      end if;
   end Action_Entity;

   ----------------------
   -- Add_Action_Group --
   ----------------------

   procedure Add_Action_Group
     (Grammar : in out Aquarius_Grammar_Record;
      Name    : in     String;
      Trigger : in     Aquarius.Actions.Action_Execution_Trigger;
      Group   :    out Aquarius.Actions.Action_Group)
   is
   begin
      Aquarius.Actions.Create_Action_Group
        (Grammar.Action_Groups, Name, Trigger, Group);
   end Add_Action_Group;

   ------------------------
   -- Add_Class_Terminal --
   ------------------------

   procedure Add_Class_Terminal
     (Grammar      : in out Aquarius_Grammar_Record;
      Declaration  : in     Aquarius.Trees.Tree;
      Name         : in     String;
      Lex          : in     Aquarius.Lexers.Lexer;
      Delimiter           : in     Boolean           := False)
   is
      use Aquarius.Tokens;
      New_Syntax  : Aquarius.Syntax.Syntax_Tree;
      New_Class   : Token_Class;
      Class_Token : Token;
   begin
      if Grammar.Non_Terminals.Contains (Name) then
         Aquarius.Errors.Error
           (Declaration,
            Grammar.Non_Terminals.Element (Name).Declaration,
            "redefinition of class terminal '" & Name & "'",
            "original definition of '" & Name & "'");
         return;
      end if;

      if Aquarius.Lexers.Matches_New_Line (Lex) then
         Grammar.Match_EOL := True;
      end if;

      Create_Token_Class (Grammar.Frame, Name, Delimiter, Lex, New_Class);
      Class_Token := Get_Class_Token (Grammar.Frame, New_Class);

      New_Syntax :=
        Aquarius.Syntax.New_Terminal (Grammar.Frame,
                                      Declaration, Class_Token);

      Grammar.Non_Terminals.Insert (Name, New_Syntax);

   end Add_Class_Terminal;

   ----------------------
   -- Add_Non_Terminal --
   ----------------------

   function Add_Non_Terminal
     (Grammar       : in out Aquarius_Grammar_Record;
      Declaration   : in Aquarius.Trees.Tree;
      Name          : in String)
      return Aquarius.Syntax.Syntax_Tree
   is
      use Aquarius.Tokens, Aquarius.Syntax;
      Non_Terminal : Syntax_Tree;
   begin
      if not Grammar.Non_Terminals.Contains (Name) then
         Non_Terminal := New_Sequence (Grammar.Frame, Declaration);
         Grammar.Non_Terminals.Insert (Name, Non_Terminal);
      else
         Non_Terminal := Grammar.Non_Terminals.Element (Name);
         if Non_Terminal.Has_Token then
            Aquarius.Errors.Error
              (Declaration,
               Non_Terminal.Declaration,
               "non-terminal cannot complete definition " &
                 "of terminal",
               "first use of '" & Name & "'");
         elsif Non_Terminal.Has_Children then
            Aquarius.Errors.Error
              (Declaration,
               Non_Terminal.Declaration,
               "redefinition of non-terminal '" & Name & "'",
               "original definition");
         else
            Non_Terminal.Set_Declaration (Declaration);
         end if;
      end if;

      if Grammar.Top_Level_Syntax = null then
         Grammar.Top_Level_Syntax := Non_Terminal;
      end if;

      Non_Terminal.Set_Non_Terminal_Name (Name);

      return Non_Terminal;

   end Add_Non_Terminal;

   ----------------------
   -- Add_Non_Terminal --
   ----------------------

   procedure Add_Non_Terminal
     (Grammar       : not null access Aquarius_Grammar_Record;
      Name          : in     String;
      Definition    : in     Aquarius.Syntax.Syntax_Tree;
      Child         : in     Aquarius.Syntax.Syntax_Tree)
   is
   begin
      Add_Non_Terminal (Grammar, Name, Definition,
                        (1 => Child));
   end Add_Non_Terminal;

   ----------------------
   -- Add_Non_Terminal --
   ----------------------

   procedure Add_Non_Terminal
     (Grammar       : not null access Aquarius_Grammar_Record;
      Name          : in     String;
      Definition    : in     Aquarius.Syntax.Syntax_Tree;
      Children      : in     Array_Of_Syntax_Trees)
   is
   begin

      if Grammar.Non_Terminals.Contains (Name) then
         declare
            S : constant Aquarius.Syntax.Syntax_Tree :=
              Grammar.Non_Terminals.Element (Name);
         begin
            --  Definition.Set_Format (S.Get_Format);
            --  S.Set_Format (Aquarius.Formats.Default_Non_Terminal_Format);
            S.Add_Child (Definition);
         end;
      else
         Grammar.Non_Terminals.Insert (Name, Definition);
         Definition.Set_Non_Terminal_Name (Name);
      end if;
      for I in Children'Range loop
         Definition.Add_Child (Children (I));
      end loop;
      declare
         use type Aquarius.Syntax.Syntax_Tree;
      begin
         if Grammar.Top_Level_Syntax = null then
            Grammar.Top_Level_Syntax := Definition;
         end if;
      end;
   end Add_Non_Terminal;

   ------------------
   -- Add_Terminal --
   ------------------

--     procedure Add_Terminal
--       (Grammar       : not null access Aquarius_Grammar_Record;
--        Name          : in     String;
--        Declaration   : in     Aquarius.Trees.Tree;
--        Definition    : in     Aquarius.Syntax.Syntax_Tree)
--     is
--        Result      : constant Aquarius.Syntax.Syntax_Tree :=
--          Add_Non_Terminal (Grammar, Declaration, Name);
--     begin
--        Result.Add_Child (Definition);
--     end Add_Terminal;

   ---------------
   -- Add_Value --
   ---------------

   procedure Add_Value (Grammar     : in out Aquarius_Grammar_Record;
                        Declaration : in     Aquarius.Trees.Tree;
                        Name        : in     String;
                        Value       : in     String)
   is
   begin
      if Name = "case_sensitive" then
         begin
            Grammar.Case_Sensitive := Boolean'Value (Value);
            Aquarius.Tokens.Set_Case_Sensitive (Grammar.Frame,
                                                Grammar.Case_Sensitive);
         exception
            when Constraint_Error =>
               Aquarius.Errors.Error
                 (Declaration,
                  "case_sensitive expects a Boolean value");
         end;
      elsif Name = "line_comment" then
         Line_Comment (Grammar, Value);
      elsif Name = "block_comment_start" then
         Block_Comment_Start (Grammar, Value);
      elsif Name = "block_comment_end" then
         Block_Comment_End (Grammar, Value);
      elsif Name = "continuation" then
         if Value'Length /= 1 then
            Aquarius.Errors.Error
              (Declaration,
               "continuation must be a single character");
         end if;
         Grammar.Continuation := Value (Value'First);
      else
         Aquarius.Errors.Warning
           (Declaration,
            "unknown setting: " & Name);
      end if;
   end Add_Value;

   -----------------------
   -- Block_Comment_End --
   -----------------------

   function Block_Comment_End
     (Grammar : in Aquarius_Grammar_Record)
      return String
   is
   begin
      return Aquarius.Names.To_String (Grammar.Block_Comment_End);
   end Block_Comment_End;

   -----------------------
   -- Block_Comment_End --
   -----------------------

   procedure Block_Comment_End
     (Grammar : in out Aquarius_Grammar_Record;
      Text    : in     String)
   is
   begin
      Grammar.Block_Comment_End := Aquarius.Names.To_Aquarius_Name (Text);
   end Block_Comment_End;

   -------------------------
   -- Block_Comment_Start --
   -------------------------

   procedure Block_Comment_Start
     (Grammar : in out Aquarius_Grammar_Record;
      Text    : in     String)
   is
   begin
      Grammar.Block_Comment_Start := Aquarius.Names.To_Aquarius_Name (Text);
   end Block_Comment_Start;

   -------------------------
   -- Block_Comment_Start --
   -------------------------

   function Block_Comment_Start
     (Grammar : in Aquarius_Grammar_Record)
      return String
   is
   begin
      return Aquarius.Names.To_String (Grammar.Block_Comment_Start);
   end Block_Comment_Start;

   -------------------
   -- Check_Grammar --
   -------------------

   procedure Check_Grammar (Grammar : in out Aquarius_Grammar_Record) is
      use Syntax_Map;
      use Aquarius.Syntax;
      Position : Cursor := Grammar.Non_Terminals.First;
      Has_Errors : Boolean := False;
   begin
      while Has_Element (Position) loop
         declare
            S : Syntax_Tree renames Element (Position);
         begin
            if not S.Has_Children and then not S.Has_Token then
               Aquarius.Errors.Error (S.Declaration,
                                      S.Name &
                                        ": undefined non-terminal");
               Has_Errors := True;
            elsif not S.Referenced and then
              S /= Grammar.Top_Level_Syntax and then
              not S.Has_Token
            then
               Aquarius.Errors.Warning (S.Declaration,
                                        S.Name &
                                        " is never referenced");
            end if;
         end;
         Next (Position);
      end loop;

      Grammar.Error := Has_Errors;

   end Check_Grammar;

   -----------
   -- Frame --
   -----------

   function Frame (Grammar : in Aquarius_Grammar_Record)
                   return Aquarius.Tokens.Token_Frame
   is
   begin
      return Grammar.Frame;
   end Frame;

   --------------------
   -- Get_Definition --
   --------------------

   function Get_Definition (Grammar : in Aquarius_Grammar_Record;
                            Name    : String)
                            return Aquarius.Syntax.Syntax_Tree
   is
      Local_Name    : constant String := Name;
      Terminal_Name : constant String := "'" & Name & "'";
   begin
      if Grammar.Non_Terminals.Contains (Local_Name) then
         return Grammar.Non_Terminals.Element (Local_Name);
      elsif Grammar.Terminals.Contains (Local_Name) then
         return Grammar.Terminals.Element (Local_Name);
      elsif Grammar.Terminals.Contains (Terminal_Name) then
         return Grammar.Terminals.Element (Terminal_Name);
      else
         return null;
      end if;
   end Get_Definition;

   -------------------
   -- Get_EBNF_Tree --
   -------------------

   function Get_EBNF_Tree
     (Grammar : Aquarius_Grammar_Record'Class)
      return Aquarius.Programs.Program_Tree
   is
   begin
      return Grammar.Definition;
   end Get_EBNF_Tree;

   ------------------------
   -- Get_Property_Types --
   ------------------------

   overriding
   function Get_Property_Types (From : Aquarius_Grammar_Record)
                                return Aquarius.Properties.Property_Type_List
   is
   begin
      return From.Properties;
   end Get_Property_Types;

   --------------------------
   -- Get_Top_Level_Syntax --
   --------------------------

   function Get_Top_Level_Syntax
     (Grammar : in Aquarius_Grammar_Record)
      return Aquarius.Syntax.Syntax_Tree
   is
   begin
      if Aquarius.Syntax."=" (Grammar.Top_Level_Syntax, null) then
         raise Constraint_Error with
           Grammar.Name & ": top level syntax was null";
      end if;
      return Grammar.Top_Level_Syntax;
   end Get_Top_Level_Syntax;

   -----------
   -- Group --
   -----------

   function Group
     (Grammar : Aquarius_Grammar_Record'Class;
      Name    : String)
      return Aquarius.Actions.Action_Group
   is
   begin
      return Aquarius.Actions.Get_Group (Grammar.Action_Groups, Name);
   end Group;

   ----------------
   -- Has_Errors --
   ----------------

   function Has_Errors (Grammar : in Aquarius_Grammar_Record)
                        return Boolean
   is
   begin
      return Grammar.Error;
   end Has_Errors;

   ------------------------
   -- Have_Block_Comment --
   ------------------------

   function Have_Block_Comment
     (Grammar : in Aquarius_Grammar_Record)
      return Boolean
   is
      use Aquarius.Names;
   begin
      return Grammar.Block_Comment_Start /= Null_Aquarius_Name;
   end Have_Block_Comment;

   -----------------------
   -- Have_Line_Comment --
   -----------------------

   function Have_Line_Comment
     (Grammar : in Aquarius_Grammar_Record)
      return Boolean
   is
      use Aquarius.Names;
   begin
      return Grammar.Line_Comment /= Null_Aquarius_Name;
   end Have_Line_Comment;

   ------------------
   -- Line_Comment --
   ------------------

   procedure Line_Comment
     (Grammar : in out Aquarius_Grammar_Record;
      Text    : in     String)
   is
   begin
      Grammar.Line_Comment := Aquarius.Names.To_Aquarius_Name (Text);
   end Line_Comment;

   ------------------
   -- Line_Comment --
   ------------------

   function Line_Comment
     (Grammar : in Aquarius_Grammar_Record)
      return String
   is
   begin
      return Aquarius.Names.To_String (Grammar.Line_Comment);
   end Line_Comment;

   --------------------
   -- Line_Continues --
   --------------------

   function Line_Continues
     (Grammar : Aquarius_Grammar_Record;
      Line    : String)
      return Natural
   is
   begin
      if not Grammar.Match_EOL
        or else Grammar.Continuation = Character'Val (0)
        or else Line'Length <= 1
        or else (Grammar.Have_Line_Comment
                 and then Ada.Strings.Fixed.Index
                   (Line, Line_Comment (Grammar)) > 0)
      then
         return 0;
      end if;

      declare
         Index : Positive := Line'Last;
      begin
         while Index > Line'First
           and then (Line (Index) = ' '
                     or else Line (Index) = Character'Val (10))
         loop
            Index := Index - 1;
         end loop;

         if Index >= Line'First
           and then Line (Index) = Grammar.Continuation
         then
            return Index;
         else
            return 0;
         end if;
      end;
   end Line_Continues;

   ---------------------
   -- Make_Error_Tree --
   ---------------------

   function Make_Error_Tree
     (Grammar  : Aquarius_Grammar_Record;
      Position : Aquarius.Source.Source_Position;
      Message  : String)
      return Aquarius.Programs.Program_Tree
   is
      Result : constant Aquarius.Programs.Program_Tree :=
        Aquarius.Programs.New_Error_Tree (Position,
                                          Grammar.Error_Syntax,
                                          Message);
   begin
      return Result;
   end Make_Error_Tree;

   -----------------------
   -- Make_Program_Tree --
   -----------------------

   not overriding
   function Make_Program_Tree
     (Grammar : not null access Aquarius_Grammar_Record;
      Name    : String)
      return Aquarius.Programs.Program_Tree
   is
      Syntax : Aquarius.Syntax.Syntax_Tree;
      Result : Aquarius.Programs.Program_Tree;
      Std_Name : constant String := Name;
   begin
      if Grammar.Non_Terminals.Contains (Std_Name) then
         Syntax := Grammar.Non_Terminals.Element (Std_Name);
         Result := Aquarius.Programs.New_Program_Tree (Syntax);
         Aquarius.Trees.Properties.Set_Grammar (Result.all,
                                                Aquarius_Grammar (Grammar));
         return Result;
      else
         raise Constraint_Error with "No such non-terminal: " & Name;
      end if;
   end Make_Program_Tree;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Grammar : Aquarius_Grammar_Record)
                  return String
   is
   begin
      return Aquarius.Tokens.To_String (Grammar.Grammar_Name);
   end Name;

   -----------------
   -- New_Grammar --
   -----------------

   function New_Grammar (Name : String;
                         EBNF : Aquarius.Programs.Program_Tree)
                        return Aquarius_Grammar
   is
      use Aquarius.Tokens;
      use Aquarius.Actions;

      Grammar : constant Aquarius_Grammar :=
                  new Aquarius_Grammar_Record'
                    (Grammar_Name        => To_Token_Text (Name),
                     Frame               => New_Frame (False),
                     Definition          => EBNF,
                     Top_Level_Syntax    => null,
                     Non_Terminals       => Syntax_Map.Empty_Map,
                     Terminals           => Syntax_Map.Empty_Map,
                     Action_Groups       => Empty_Action_Group_List,
                     Action_Entities     => <>,
                     Case_Sensitive      => False,
                     Match_EOL           => False,
                     Continuation        => Character'Val (0),
                     Line_Comment        => Aquarius.Names.Null_Aquarius_Name,
                     Block_Comment_Start => Aquarius.Names.Null_Aquarius_Name,
                     Block_Comment_End   => Aquarius.Names.Null_Aquarius_Name,
                     Error_Token         => Aquarius.Tokens.Null_Token,
                     Error_Syntax        => null,
                     Error               => False,
                     Properties          => Aquarius.Properties.Empty_Pool);

   begin

      declare
         Error_Class : Aquarius.Tokens.Token_Class;
      begin
         Aquarius.Tokens.Create_Token_Class (Grammar.Frame, "error", False,
                                             Aquarius.Lexers.Null_Lexer,
                                             Error_Class);
         Grammar.Error_Token :=
           Aquarius.Tokens.Get_Class_Token (Grammar.Frame, Error_Class);
      end;

      Grammar.Error_Syntax :=
        Aquarius.Syntax.New_Terminal (Grammar.Frame,
                                      Aquarius.Trees.Internal_Declaration,
                                      Grammar.Error_Token);

      return Grammar;
   end New_Grammar;

   ------------------
   -- New_Property --
   ------------------

   function New_Property
     (Grammar   : not null access Aquarius_Grammar_Record'Class;
      Name      : String;
      Inherited : Boolean;
      Has_Value : Boolean)
      return Aquarius.Properties.Property_Type
   is
      Result : Aquarius.Properties.Property_Type;
   begin
      Aquarius.Properties.Create_Property
        (Grammar.all, Result, Name,
         Inherited, Has_Value);
      return Result;
   end New_Property;

   --------------------
   -- Reference_Name --
   --------------------

   function Reference_Name
     (Grammar      : in out Aquarius_Grammar_Record;
      Reference    : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Name         : in     String;
      Indent_Rule  : in     Boolean       := False;
      Offset_Rule  : in     Boolean       := False)
      return Aquarius.Syntax.Syntax_Tree
   is
      pragma Unreferenced (Offset_Rule);  --  we haven't implemented it yet
      Local_Name : constant String := Name;
      Result     : Aquarius.Syntax.Syntax_Tree;
   begin
      if Grammar.Non_Terminals.Contains (Local_Name) then
         Result := Grammar.Non_Terminals.Element (Local_Name);
         Result.Set_Referenced;
      else
         Result := Aquarius.Syntax.New_Sequence (Grammar.Frame, Reference);
         Result.Set_Non_Terminal_Name (Name);
         Result.Set_Referenced;
         Grammar.Non_Terminals.Insert (Local_Name, Result);
      end if;

      if Indent_Rule then
         Result.Enable_Indent_Rule;
      end if;

      return Result;

   end Reference_Name;

   ------------------------
   -- Reference_Terminal --
   ------------------------

   function Reference_Terminal
     (Grammar           : in out Aquarius_Grammar_Record;
      Reference         : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Text              : in     String;
      Indent_Rule       : in     Boolean       := False;
      Offset_Rule       : in     Boolean       := False)
      return Aquarius.Syntax.Syntax_Tree
   is
      pragma Unreferenced (Offset_Rule);
      Tok    : Aquarius.Tokens.Token;
      Result : Aquarius.Syntax.Syntax_Tree;
      Local_Name       : constant String := Text;
   begin
      if Aquarius.Tokens.Exists (Grammar.Frame, Text) then
         Tok := Aquarius.Tokens.Get_Token (Grammar.Frame, Text);
      else
         Aquarius.Tokens.Create_Reserved_Token
           (Grammar.Frame, Text, Tok);
      end if;

      if Grammar.Terminals.Contains (Local_Name) then
         Result := Grammar.Terminals.Element (Local_Name);
      else
         Result := Aquarius.Syntax.New_Terminal (Grammar.Frame, Reference,
                                                 Tok);
         Grammar.Terminals.Insert (Local_Name, Result);
      end if;

      if Indent_Rule then
         Result.Enable_Indent_Rule;
      end if;

      return Result;
   end Reference_Terminal;

   ------------------------
   -- Run_Action_Trigger --
   ------------------------

   procedure Run_Action_Trigger
     (Grammar    : Aquarius_Grammar_Record;
      Start      : Aquarius.Programs.Program_Tree;
      Trigger    : Aquarius.Actions.Action_Execution_Trigger;
      Stop_After : Aquarius.Programs.Program_Tree := null)
   is
      procedure Run_Group_Actions (Group : Aquarius.Actions.Action_Group);

      -----------------------
      -- Run_Group_Actions --
      -----------------------

      procedure Run_Group_Actions (Group : Aquarius.Actions.Action_Group) is
      begin
         Start.Run_Actions (Group, Stop_After);
      end Run_Group_Actions;

   begin
      Aquarius.Actions.Iterate
        (Grammar.Action_Groups, Trigger, Run_Group_Actions'Access);
   end Run_Action_Trigger;

   ------------------------
   -- Run_Action_Trigger --
   ------------------------

   procedure Run_Action_Trigger
     (Grammar    : Aquarius_Grammar_Record;
      Start      : Aquarius.Programs.Program_Tree;
      Stop       : Aquarius.Programs.Program_Tree;
      Trigger    : Aquarius.Actions.Action_Execution_Trigger)
   is
      procedure Run_Group_Actions (Group : Aquarius.Actions.Action_Group);

      -----------------------
      -- Run_Group_Actions --
      -----------------------

      procedure Run_Group_Actions (Group : Aquarius.Actions.Action_Group) is
      begin
         Start.Run_Actions (Stop, Group);
      end Run_Group_Actions;

   begin

      Aquarius.Actions.Iterate
        (Grammar.Action_Groups, Trigger, Run_Group_Actions'Access);

   exception
      when others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "exception caught while running actions");
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "grammar: " & Grammar.Name);
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "start  : " & Start.Image);
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "stop   : " & Stop.Image);
         raise;
   end Run_Action_Trigger;

   -----------------
   -- Run_Actions --
   -----------------

   procedure Run_Actions
     (Grammar      : in Aquarius_Grammar_Record;
      Group_Name   : in String;
      Start        : in Aquarius.Programs.Program_Tree)
   is
   begin

      if Aquarius.Actions.Have_Group
        (Grammar.Action_Groups, Group_Name)
      then
         declare
            Group : constant Aquarius.Actions.Action_Group :=
                      Aquarius.Actions.Get_Group
                        (Grammar.Action_Groups, Group_Name);
         begin
            Start.Run_Actions (Group);
         end;
      else
         declare
            use Aquarius.Actions;
            Trigger : Action_Execution_Trigger;
         begin
            Trigger :=
              Action_Execution_Trigger'Value (Group_Name & "_Trigger");
            Grammar.Run_Action_Trigger (Start, Trigger);
         exception
            when Constraint_Error =>
               raise Constraint_Error with
               Group_Name & " is neither an action group nor a trigger";
         end;
      end if;
   end Run_Actions;

   -----------------------
   -- Run_Parse_Actions --
   -----------------------

   procedure Run_Parse_Actions
     (Tree     : in out Aquarius.Programs.Program_Tree_Type'Class;
      Position : in Rule_Position)
   is

      Grammar : constant Aquarius_Grammar :=
        Aquarius.Trees.Properties.Get_Grammar (Tree);

      procedure Run_Group_Actions (Group : Aquarius.Actions.Action_Group);

      -----------------------
      -- Run_Group_Actions --
      -----------------------

      procedure Run_Group_Actions (Group : Aquarius.Actions.Action_Group) is
      begin
         Tree.Execute_Single_Action (Group, Position);
      end Run_Group_Actions;

   begin

      Aquarius.Actions.Iterate
        (Grammar.Action_Groups, Aquarius.Actions.Parse_Trigger,
         Run_Group_Actions'Access);
   end Run_Parse_Actions;

   -----------------------------
   -- Significant_End_Of_Line --
   -----------------------------

   function Significant_End_Of_Line
     (Grammar : Aquarius_Grammar_Record)
      return Boolean
   is
   begin
      return Grammar.Match_EOL;
   end Significant_End_Of_Line;

end Aquarius.Grammars;
