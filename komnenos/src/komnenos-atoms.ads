private with Ada.Strings.Unbounded;

package Komnenos.Atoms is

   type Root_Atom_Type is tagged private;

   function Foreground_Colour (Atom : Root_Atom_Type) return String;
   function Background_Colour (Atom : Root_Atom_Type) return String;
   function Font_Name (Atom : Root_Atom_Type) return String;
   function Font_Size (Atom : Root_Atom_Type) return Natural;
   function Bold (Atom : Root_Atom_Type) return Boolean;
   function Italic (Atom : Root_Atom_Type) return Boolean;
   function Strike_Through (Atom : Root_Atom_Type) return Boolean;

   function Clickable (Atom : Root_Atom_Type) return Boolean;
   procedure On_Click
     (Atom : in out Root_Atom_Type)
   is null;

   function Text (Atom : Root_Atom_Type) return String;

   type Atom_Type is access all Root_Atom_Type'Class;

   function New_Text_Atom (Text : String) return Atom_Type;

private

   type Root_Atom_Type is tagged
      record
         Foreground_Colour : access String;
         Background_Colour : access String;
         Font_Name         : access String;
         Font_Size         : Natural := 0;
         Bold, Italic      : Boolean        := False;
         Strike_Through    : Boolean        := False;
         Clickable         : Boolean        := False;
         Text              : Ada.Strings.Unbounded.Unbounded_String;
      end record;

end Komnenos.Atoms;
