package body Komnenos.Atoms is

   -----------------------
   -- Background_Colour --
   -----------------------

   function Background_Colour (Atom : Root_Atom_Type) return String is
   begin
      if Atom.Background_Colour = null then
         return "";
      else
         return Atom.Background_Colour.all;
      end if;
   end Background_Colour;

   ----------
   -- Bold --
   ----------

   function Bold (Atom : Root_Atom_Type) return Boolean is
   begin
      return Atom.Bold;
   end Bold;

   ---------------
   -- Clickable --
   ---------------

   function Clickable (Atom : Root_Atom_Type) return Boolean is
   begin
      return Atom.Clickable;
   end Clickable;

   ---------------
   -- Font_Name --
   ---------------

   function Font_Name (Atom : Root_Atom_Type) return String is
   begin
      if Atom.Font_Name = null then
         return "";
      else
         return Atom.Font_Name.all;
      end if;
   end Font_Name;

   ---------------
   -- Font_Size --
   ---------------

   function Font_Size (Atom : Root_Atom_Type) return Natural is
   begin
      return Atom.Font_Size;
   end Font_Size;

   -----------------------
   -- Foreground_Colour --
   -----------------------

   function Foreground_Colour (Atom : Root_Atom_Type) return String is
   begin
      if Atom.Foreground_Colour = null then
         return "";
      else
         return Atom.Foreground_Colour.all;
      end if;
   end Foreground_Colour;

   ------------
   -- Italic --
   ------------

   function Italic (Atom : Root_Atom_Type) return Boolean is
   begin
      return Atom.Italic;
   end Italic;

   -------------------
   -- New_Text_Atom --
   -------------------

   function New_Text_Atom (Text : String) return Atom_Type is
      Result : constant Atom_Type := new Root_Atom_Type;
   begin
      Result.Text := Ada.Strings.Unbounded.To_Unbounded_String (Text);
      return Result;
   end New_Text_Atom;

   --------------------
   -- Strike_Through --
   --------------------

   function Strike_Through (Atom : Root_Atom_Type) return Boolean is
   begin
      return Atom.Strike_Through;
   end Strike_Through;

   ----------
   -- Text --
   ----------

   function Text (Atom : Root_Atom_Type) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Atom.Text);
   end Text;

end Komnenos.Atoms;
