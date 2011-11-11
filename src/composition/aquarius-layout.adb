with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body Aquarius.Layout is

   -------
   -- + --
   -------

   function "+" (Pos     : Position;
                 Columns : Count)
                return Position
   is
   begin
      return (Pos.Line, Pos.Column + Columns);
   end "+";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Position) return Boolean is
   begin
      if Left.Line < Right.Line then
         return True;
      elsif Left.Line = Right.Line then
         return Left.Column < Right.Column;
      else
         return False;
      end if;
   end "<";

   --------
   -- <= --
   --------

   function "<=" (Left, Right : Position) return Boolean is
   begin
      return Left < Right or else Left = Right;
   end "<=";

   -------
   -- > --
   -------

   function ">" (Left, Right : Position) return Boolean is
   begin
      return not (Left <= Right);
   end ">";

   --------
   -- >= --
   --------

   function ">=" (Left, Right : Position) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   ----------
   -- Fill --
   ----------

   function Fill (Text    : String;
                  Width   : Positive;
                  Justify : Boolean)
                 return String
   is
      pragma Unreferenced (Justify);
      use Ada.Strings.Unbounded;
      Previous_New_Line : Natural := 0;
      Previous_Space    : Natural := 0;
      Result            : Unbounded_String := Null_Unbounded_String;
   begin
      for I in Text'Range loop
         if I - Previous_New_Line > Width then
            if Previous_Space > 0 then
               Result :=
                 Result & Text (Previous_New_Line + 1 .. Previous_Space - 1);
               Previous_New_Line := Previous_Space;
            else
               Result := Result & Text (Previous_New_Line .. I);
               Previous_New_Line := I - 1;
            end if;
            Result := Result & Character'Val (10);
            Previous_Space := 0;
         elsif Text (I) = ' ' then
            Previous_Space := I;
         end if;
      end loop;
      if Previous_New_Line = 0 then
         return Text;
      elsif Previous_New_Line < Text'Last then
         Result := Result & Text (Previous_New_Line .. Text'Last);
      end if;
      return To_String (Result);
   end Fill;

   ----------
   -- Show --
   ----------

   function Show (Pos : Position) return String is
   begin
      return "(" & Ada.Strings.Fixed.Trim (Pos.Line'Img, Ada.Strings.Both) &
        "," & Ada.Strings.Fixed.Trim (Pos.Column'Img, Ada.Strings.Both) &
        ")";
   end Show;

end Aquarius.Layout;
