--  with Ada.Strings.Fixed;

with Aquarius.Themes;

with Komnenos.Fragments.Notes;
with Komnenos.UI;

package body Komnenos.Entities.Source is

   function Placeholder_Text
     (Entity : Root_Source_Entity_Reference'Class)
      return String;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item         : in out Root_Source_Entity_Reference'Class;
      Name         : String;
      File_Name    : String;
      Class        : String;
      Line         : Natural;
      Column       : Natural)
   is
      use Ada.Strings, Ada.Strings.Fixed;
--        Line_Text   : constant String := Trim (Natural'Image (Line), Both);
--        Column_Text : constant String := Trim (Natural'Image (Column), Both);
   begin
      Root_Entity_Reference (Item).Create
        (Identifier   => Name,
         Class_Name   => Class,
         Display_Text => Name,
         Description  => Name);
      Item.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Item.File_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (File_Name);
      Item.Line := Line;
      Item.Column := Column;
   end Create;

   -----------------------
   -- New_Source_Entity --
   -----------------------

   function New_Source_Entity
     (Name         : String;
      File_Name    : String;
      Class        : String;
      Line         : Natural;
      Column       : Natural)
      return Entity_Reference
   is
      Result : Root_Source_Entity_Reference;
   begin
      Result.Create (Name, File_Name, Class, Line, Column);
      return new Root_Source_Entity_Reference'(Result);
   end New_Source_Entity;

   ----------------------
   -- Placeholder_Text --
   ----------------------

   function Placeholder_Text
     (Entity : Root_Source_Entity_Reference'Class)
      return String
   is
      use Ada.Strings.Unbounded;
      S : constant Unbounded_String :=
            Entity.Name
            & " " & Class (Entity)
            & " defined in "
            & To_String (Entity.File_Name)
            & " line"
            & Natural'Image (Entity.Line)
            & " column"
            & Natural'Image (Entity.Column);
   begin
      return To_String (S);
   end Placeholder_Text;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Entity : not null access Root_Source_Entity_Reference;
      Visual : not null access Entity_Visual'Class)
   is
   begin
      Visual.Clear;
      Visual.Put_Line (Placeholder_Text (Entity.all),
                       Aquarius.Themes.Active_Theme.Default_Style);
   end Render;

   -------------------
   -- Select_Entity --
   -------------------

   overriding procedure Select_Entity
     (Entity : not null access Root_Source_Entity_Reference;
      Table  : access Entity_Table_Interface'Class;
      Parent : access Entity_Visual'Class;
      Visual : access Entity_Visual'Class;
      Offset : Natural)
   is
      pragma Unreferenced (Table);
      use Ada.Strings.Unbounded;
      Note : constant Komnenos.Fragments.Fragment_Type :=
               (if Visual = null
                then Komnenos.Fragments.Notes.New_Note_Fragment
                  (Placeholder_Text (Entity.all))
                else Komnenos.Fragments.Fragment_Type (Visual));
   begin
      Komnenos.UI.Current_UI.Place_Fragment
        (Parent, Offset, Note);
   end Select_Entity;

end Komnenos.Entities.Source;
