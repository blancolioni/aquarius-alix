private with Aquarius.Counters;

with Aquarius.Grammars;
with Aquarius.Source;
with Aquarius.Tokens;
with Aquarius.Trees.Cursors;

private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

package Aquarius.Programs.Parser is

   type Parse_Context is private;

   procedure Initialise_Parse_Context
     (Context     : out Parse_Context;
      Grammar     : in  Aquarius.Grammars.Aquarius_Grammar;
      Root        : in  Program_Tree;
      Interactive : in  Boolean;
      Run_Actions : in  Boolean := True);

   function Get_Cursor (Context : Parse_Context)
                       return Aquarius.Trees.Cursors.Cursor;

   procedure Set_Cursor (Context : in out Parse_Context;
                         Cursor  : in     Aquarius.Trees.Cursors.Cursor);

   procedure Add_Comment (Context  : in out Parse_Context;
                          Position : Aquarius.Source.Source_Position;
                          Comment  : in     Program_Tree);

   procedure Clear_Comments (Context  : in out Parse_Context);

   procedure Add_Error (Context : in out Parse_Context;
                        Error   : in     Program_Tree);

   procedure Finish_Parse (Context : in out Parse_Context);

   function Is_Ambiguous (Context : Parse_Context) return Boolean;

   function Token_OK
     (Tok            : in Aquarius.Tokens.Token;
      Tok_Pos        : in Aquarius.Source.Source_Position;
      Context        : in Parse_Context)
     return Boolean;

   procedure Parse_Token
     (Tok            : in     Aquarius.Tokens.Token;
      Tok_Pos        : in     Aquarius.Source.Source_Position;
      Tok_Text       : in     String;
      Context        : in out Parse_Context);

--     procedure Backtrack_Parser
--       (Tok            : in     Aquarius.Tokens.Token;
--        Tok_Pos        : in     Aquarius.Source.Source_Position;
--        Tok_Text       : in     String;
--        Context        : in out Parse_Context;
--        Recovered      :    out Boolean);

   procedure Set_Vertical_Space (Context  : in out Parse_Context;
                                 Space    : in     Natural);

   procedure Parse_Tree (Top  : in Program_Tree;
                         Code : in String);

   procedure Parse_Tree (Top  : in Program_Tree;
                         Tree : in Program_Tree);

   procedure Parse_Tree (Top    : in Program_Tree;
                         Before : in String;
                         Child  : in Aquarius.Programs.Program_Tree;
                         After  : in String);

   procedure Repeat_Child (Top   : in Program_Tree;
                           Child : in Array_Of_Program_Trees);

   function Tree_OK
     (Tree           : in Program_Tree;
      Context        : in Parse_Context)
     return Boolean;

   procedure Parse_Tree
     (Tree           : in     Program_Tree;
      Context        : in out Parse_Context);

   procedure Summarise_Context
     (Context : Parse_Context);

private

   type Ambiguity_Record;

   type Ambiguity is access Ambiguity_Record;

   package List_Of_Ambiguities is
      new Ada.Containers.Doubly_Linked_Lists (Ambiguity);

   type Ambiguity_Record is
      record
         Active     : Boolean;
         Identity   : Aquarius.Counters.Counter_Type;
         Parent     : Program_Tree;
         Right      : Program_Tree;
         Top        : Program_Tree;
         Last_Parse : Program_Tree;
         Location   : Aquarius.Trees.Cursors.Cursor;
         Previous   : List_Of_Ambiguities.Cursor;
      end record;

   package Program_Tree_Vector is
      new Ada.Containers.Vectors (Positive, Program_Tree);

   type Parse_Context is
      record
         Grammar         : Aquarius.Grammars.Aquarius_Grammar;
         Ambiguities     : List_Of_Ambiguities.List;
         Comments        : Program_Tree_Vector.Vector;
         Errors          : Program_Tree_Vector.Vector;
         Interactive     : Boolean                         := True;
         Vertical_Space  : Natural                         := 0;
         Run_Actions     : Boolean                         := True;
      end record;

end Aquarius.Programs.Parser;
