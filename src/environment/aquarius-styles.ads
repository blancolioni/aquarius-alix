private with Ada.Containers.Vectors;

with Aquarius.Fonts;

package Aquarius.Styles is

   type Aquarius_Root_Style is tagged private;

   type Aquarius_Style is access all Aquarius_Root_Style'Class;

   function Font (Style : access Aquarius_Root_Style;
                  Class : in     String)
                 return Aquarius.Fonts.Aquarius_Font;

   function Default_Style return Aquarius_Style;

   function Load_Style (Name : String) return Aquarius_Style;

private

   subtype Style_Entry_Name is String (1 .. 20);
   type Style_Entry is
      record
         Name : Style_Entry_Name;
         Font : Aquarius.Fonts.Aquarius_Font;
      end record;

   package Style_Entry_Vectors is
     new Ada.Containers.Vectors (Positive, Style_Entry);

   type Aquarius_Root_Style is tagged
      record
         Entries      : Style_Entry_Vectors.Vector;
         Default_Font : Aquarius.Fonts.Aquarius_Font;
      end record;

end Aquarius.Styles;
