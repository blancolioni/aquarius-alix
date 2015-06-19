private with Ada.Containers.Vectors;

with Aquarius.Names;
with Aquarius.Styles;

package Aquarius.Themes is

   type Element_State is (Normal, Hover, Selected, Disabled);

   type Aquarius_Root_Theme is tagged private;

   type Aquarius_Theme is access all Aquarius_Root_Theme'Class;

   Null_Theme : constant Aquarius_Theme := null;
   function Active_Theme return Aquarius_Theme;

   function Style (Theme : access Aquarius_Root_Theme;
                   Class : in     String;
                   State :        Element_State := Normal)
                   return Aquarius.Styles.Aquarius_Style;

   function Default_Style
     (Theme : Aquarius_Root_Theme)
      return Aquarius.Styles.Aquarius_Style;

   function Default_Font_Name
     (Theme : Aquarius_Root_Theme)
      return String;

   function Default_Font_Size
     (Theme : Aquarius_Root_Theme)
      return Natural;

   function Load_Theme (Name : String) return Aquarius_Theme;

private

   type Theme_Entry is
      record
         Class : Aquarius.Names.Aquarius_Name;
         State : Element_State;
         Style : Aquarius.Styles.Aquarius_Style;
      end record;

   package Theme_Entry_Vectors is
     new Ada.Containers.Vectors (Positive, Theme_Entry);

   type Aquarius_Root_Theme is tagged
      record
         Entries           : Theme_Entry_Vectors.Vector;
         Default_Style     : Aquarius.Styles.Aquarius_Style;
         Default_Font_Name : Aquarius.Names.Aquarius_Name;
         Default_Font_Size : Natural;
      end record;

end Aquarius.Themes;
