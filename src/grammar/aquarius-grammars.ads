with Aquarius.Actions;
with Aquarius.Lexers;
with Aquarius.Programs;
with Aquarius.Properties;
with Aquarius.Source;
with Aquarius.Syntax;
with Aquarius.Tokens;
with Aquarius.Trees;
with Aquarius.UI.Menus;

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Hash_Case_Insensitive;
private with Ada.Strings.Equal_Case_Insensitive;

package Aquarius.Grammars is

   Grammar_Error : exception;

   type Aquarius_Grammar_Record is
     new Root_Aquarius_Object and
     Aquarius.Properties.Property_Pool
     with private;

   type Aquarius_Grammar is access all Aquarius_Grammar_Record'Class;

   not overriding
   function New_Grammar (Name : String)
                        return Aquarius_Grammar;

   overriding
   function Name (Grammar : Aquarius_Grammar_Record)
                 return String;

   not overriding
   procedure Add_Class_Terminal
     (Grammar      : in out Aquarius_Grammar_Record;
      Declaration  : in     Aquarius.Trees.Tree;
      Name         : in     String;
      Lex          : in     Aquarius.Lexers.Lexer;
      Delimiter    : in     Boolean                   := False;
      Line_Comment : in     Boolean                   := False);

   not overriding
   procedure Add_Value (Grammar     : in out Aquarius_Grammar_Record;
                        Declaration : in     Aquarius.Trees.Tree;
                        Name        : in     String;
                        Value       : in     String);

   not overriding
   function Add_Non_Terminal
     (Grammar       : in out Aquarius_Grammar_Record;
      Declaration   : in Aquarius.Trees.Tree;
      Name          : in String)
     return Aquarius.Syntax.Syntax_Tree;

   --  Reference_Name: create a syntax rule that references a named
   --  entity, which may or may not have been defined yet.  Return
   --  a syntax tree which represents the reference (and indirectly,
   --  the named entity).

   --     Grammar      : current grammar
   --     Name         : name of the rule
   --     Indent_Rule  : True if the referenced rule is bounded by
   --                    a non-zero indent (a token is only considered
   --                    as a potential continuation of the rule if it
   --                    is a non-zero number of characters from the
   --                    left-most column).
   --     Offset_Rule  : True if the referenced rule is bounded by
   --                    by an offset rule (a token is only considered
   --                    as a continuation of the rule if it is as least
   --                    as far to the right as the first element of the
   --                    rule)  (not currently implemented).

   not overriding
   function Reference_Name
     (Grammar      : in out Aquarius_Grammar_Record;
      Reference    : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Name         : in     String;
      Indent_Rule  : in     Boolean       := False;
      Offset_Rule  : in     Boolean       := False)
     return Aquarius.Syntax.Syntax_Tree;

   --  Reference_Token: report a reference to a reserved token.
   --  If it doesn't already exist, it will be added with the
   --  appropriate class

   --  Text: the literal text of this terminal
   --  Non_Terminal_Name: the name of the associated non-terminal

   not overriding
   function Reference_Terminal
     (Grammar           : in out Aquarius_Grammar_Record;
      Reference         : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Text              : in     String;
      Indent_Rule       : in     Boolean       := False;
      Offset_Rule       : in     Boolean       := False)
     return Aquarius.Syntax.Syntax_Tree;

   not overriding
   function Get_Definition (Grammar : in Aquarius_Grammar_Record;
                            Name    : String)
                           return Aquarius.Syntax.Syntax_Tree;

   not overriding
   function Get_Top_Level_Syntax
     (Grammar : in Aquarius_Grammar_Record)
     return Aquarius.Syntax.Syntax_Tree;

   not overriding
   procedure Check_Grammar (Grammar : in out Aquarius_Grammar_Record);

   procedure Add_Action_Group
     (Grammar : in out Aquarius_Grammar_Record;
      Name    : in     String;
      Trigger : in     Aquarius.Actions.Action_Execution_Trigger;
      Group   :    out Aquarius.Actions.Action_Group);
   --  Add_Action_Group: add an action group to the grammar.  If more
   --  than one action group has the same trigger, (see
   --  Aquarius.Actions spec for details on triggers) they are always
   --  executed in the same order they were added.  Furthermore, an
   --  action group always runs in isolation and to completion on the
   --  entire tree before the next one the in trigger set starts.

   function Significant_End_Of_Line
     (Grammar : Aquarius_Grammar_Record)
      return Boolean;
   --  Return True if end of line is syntactically significant.
   --  This is detected automatically if the grammar has a rule which
   --  matches the end of line.

   function Group
     (Grammar : Aquarius_Grammar_Record'Class;
      Name    : String)
      return Aquarius.Actions.Action_Group;

   not overriding
   procedure Run_Actions
     (Grammar      : in Aquarius_Grammar_Record;
      Group_Name   : in String;
      Start        : in Aquarius.Programs.Program_Tree);

   --  Run_Action_Trigger: run action groups belonging to
   --  the specified trigger.  Perform them via a depth
   --  first search.  Process action groups in order of
   --  creation, finishing the first action group completely
   --  before moving to the next.  If Stop_After is not
   --  null, the run stops after the given tree has been
   --  processed.

   not overriding
   procedure Run_Action_Trigger
     (Grammar    : Aquarius_Grammar_Record;
      Start      : Aquarius.Programs.Program_Tree;
      Trigger    : Aquarius.Actions.Action_Execution_Trigger;
      Stop_After : Aquarius.Programs.Program_Tree := null);

   not overriding
   procedure Run_Action_Trigger
     (Grammar    : Aquarius_Grammar_Record;
      Start      : Aquarius.Programs.Program_Tree;
      Stop       : Aquarius.Programs.Program_Tree;
      Trigger    : Aquarius.Actions.Action_Execution_Trigger);

   --  Run_Parse_Actions: run actions at the specified Tree and Position
   --  that are triggered by parsing.

   procedure Run_Parse_Actions
     (Tree     : in out Aquarius.Programs.Program_Tree_Type'Class;
      Position : in Rule_Position);

   not overriding
   function Frame (Grammar : in Aquarius_Grammar_Record)
                  return Aquarius.Tokens.Token_Frame;

   not overriding
   function Has_Errors (Grammar : in Aquarius_Grammar_Record)
                       return Boolean;

   not overriding
   function Comment_Token (Grammar : in Aquarius_Grammar_Record)
                          return Aquarius.Tokens.Token;

   not overriding
   function Make_Program_Tree
     (Grammar : not null access Aquarius_Grammar_Record;
      Name    : String)
      return Aquarius.Programs.Program_Tree;

   not overriding
   function Make_Comment_Tree
     (Grammar : Aquarius_Grammar_Record;
      Comment : String)
     return Aquarius.Programs.Program_Tree;

   not overriding
   function Make_Error_Tree
     (Grammar  : Aquarius_Grammar_Record;
      Position : Aquarius.Source.Source_Position;
      Message  : String)
     return Aquarius.Programs.Program_Tree;

   function New_Property
     (Grammar   : not null access Aquarius_Grammar_Record'Class;
      Name      : String;
      Inherited : Boolean;
      Has_Value : Boolean)
      return Aquarius.Properties.Property_Type;

   function Get_Menu (Grammar : Aquarius_Grammar_Record'Class)
                      return Aquarius.UI.Menus.Aquarius_Menu;

private

   package Syntax_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Aquarius.Syntax.Syntax_Tree,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive,
      "="             => Aquarius.Syntax."=");

   type Aquarius_Grammar_Record is
     new Root_Aquarius_Object and
     Aquarius.Properties.Property_Pool with
      record
         Grammar_Name       : Aquarius.Tokens.Token_Text;
         Frame              : Aquarius.Tokens.Token_Frame;
         Definition         : Aquarius.Programs.Program_Tree;
         Top_Level_Syntax   : Aquarius.Syntax.Syntax_Tree;
         Actions            : Aquarius.Actions.Action_Group_List;
         Case_Sensitive     : Boolean;
         Match_EOL          : Boolean := False;
         Non_Terminals      : Syntax_Map.Map;
         Terminals          : Syntax_Map.Map;
         Comment            : Aquarius.Tokens.Token;
         Comment_Syntax     : Aquarius.Syntax.Syntax_Tree;
         Error_Token        : Aquarius.Tokens.Token;
         Error_Syntax       : Aquarius.Syntax.Syntax_Tree;
         Error              : Boolean;
         Properties         : Aquarius.Properties.Property_Type_List;
         Menu               : Aquarius.UI.Menus.Aquarius_Menu;
      end record;

   overriding
   function Get_Property_Types (From : Aquarius_Grammar_Record)
                               return Aquarius.Properties.Property_Type_List;

   --  private subprograms for child packages

   --  The following definitions of Add_Non_Terminal are only
   --  be used for bootstrapping Aquarius (i.e. when creating the
   --  the syntax for EBNF

   not overriding
   procedure Add_Non_Terminal
     (Grammar       : not null access Aquarius_Grammar_Record;
      Name          : in String;
      Definition    : in Aquarius.Syntax.Syntax_Tree;
      Child         : in Aquarius.Syntax.Syntax_Tree);

   type Array_Of_Syntax_Trees is
     array (Positive range <>) of Aquarius.Syntax.Syntax_Tree;

   not overriding
   procedure Add_Non_Terminal
     (Grammar       : not null access Aquarius_Grammar_Record;
      Name          : in String;
      Definition    : in Aquarius.Syntax.Syntax_Tree;
      Children      : in Array_Of_Syntax_Trees);

end Aquarius.Grammars;
