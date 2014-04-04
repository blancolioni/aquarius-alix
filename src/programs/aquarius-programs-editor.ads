private with Aquarius.Programs.Parser;

with Aquarius.Trees.Cursors;

package Aquarius.Programs.Editor is

   type Aquarius_Rendering_Interface is interface;

   procedure Update
     (Renderer  : in out Aquarius_Rendering_Interface;
      Point     : Aquarius.Trees.Cursors.Cursor;
      Partial   : String)
   is abstract;

   type Root_Program_Editor is tagged private;

   procedure Add_Renderer
     (Editor   : in out Root_Program_Editor;
      Renderer : access Aquarius_Rendering_Interface'Class;
      Top      : Program_Tree);

   procedure Set_Point
     (Editor : in out Root_Program_Editor;
      Point  : in     Aquarius.Layout.Position);

   procedure Insert_Character
     (Editor  : in out Root_Program_Editor;
      Ch      : Character;
      Echo    : out Boolean);

   procedure Delete_Characters
     (Editor : in out Root_Program_Editor;
      Start  : Integer;
      Finish : Integer)
   with Pre => Finish >= Start;
   --  Delete a number of characters, from (current_position + start)
   --  to (current_position + finish).
   --  If Start < 0, add Start to the current cursor position

   procedure Backspace
     (Editor : in out Root_Program_Editor'Class);
   --  Specialisation of Delete_Characters with start = finish = -1

   type Program_Editor is access all Root_Program_Editor'Class;

   function Create_Editor
     (Root : Program_Tree)
      return Program_Editor;

private

   type Input_Buffer (Size : Positive) is
      record
         Buffer   : String (1 .. Size) := (others => ' ');
         Start    : Aquarius.Layout.Position := (1, 1);
         Length   : Natural := 0;
         Cursor   : Natural := 0;
         Changed  : Boolean := False;
         Active   : Boolean := False;
      end record;

   type Root_Program_Editor is tagged
      record
         Root           : Program_Tree;
         Top            : Program_Tree;
         Left_Terminal  : Program_Tree;
         Right_Terminal : Program_Tree;
         Edit_Terminal  : Program_Tree;
         Join_Left      : Boolean := False;
         Join_Right     : Boolean := False;
         Renderer       : access Aquarius_Rendering_Interface'Class;
         Position       : Aquarius.Layout.Position;
         Source_Pos     : Aquarius.Source.Source_Position;
         Context        : Aquarius.Programs.Parser.Parse_Context;
         Input          : Input_Buffer (256);
      end record;

end Aquarius.Programs.Editor;
