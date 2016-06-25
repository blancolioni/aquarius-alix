package Aquarius.Layout is

   pragma Preelaborate (Aquarius.Layout);

   type Count is new Natural;
   subtype Positive_Count is Count range 1 .. Count'Last;

   type Position is
      record
         Line   : Positive_Count;
         Column : Positive_Count;
      end record;

   function Show (Pos : Position) return String;

   function "<" (Left, Right : Position) return Boolean;
   function "<=" (Left, Right : Position) return Boolean;
   function ">" (Left, Right : Position) return Boolean;
   function ">=" (Left, Right : Position) return Boolean;

   function "+" (Pos     : Position;
                 Columns : Count)
                 return Position;

   procedure Next (Pos : in out Position);
   --  move to next column

   function Fill (Text    : String;
                  Width   : Positive;
                  Justify : Boolean)
                  return String;
   --  Fill: split the text into lines so that it fills the given
   --  width.  If Justify is true, the right margin will be
   --  justified.

   type Selection is
      record
         Start  : Position;
         Finish : Position;
      end record;

   type Line_Offset is new Integer;
   type Character_Offset is new Integer;

end Aquarius.Layout;
