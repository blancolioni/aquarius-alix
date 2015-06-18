with Ada.Strings.Fixed;

with Komnenos.Fragments.Notes;
with Komnenos.UI;

package body Komnenos.Entities.Source is

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
      Line_Text   : constant String := Trim (Natural'Image (Line), Both);
      Column_Text : constant String := Trim (Natural'Image (Column), Both);
   begin
      Root_Entity_Reference (Item).Create
        (Identifier   => File_Name & "|" & Name,
         Class_Name   => Class,
         Display_Text => Name,
         Description  => File_Name & ":" & Line_Text & ":" & Column_Text);
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

   -------------------
   -- Select_Entity --
   -------------------

   overriding procedure Select_Entity
     (Entity : Root_Source_Entity_Reference;
      Table  : access Entity_Table_Interface'Class;
      Parent : access Entity_Visual'Class;
      Offset : Natural)
   is
      pragma Unreferenced (Table);
      use Ada.Strings.Unbounded;
      Note : constant Komnenos.Fragments.Fragment_Type :=
               Komnenos.Fragments.Notes.New_Note_Fragment
                 (To_String (Entity.Name)
                  & " " & Class (Entity)
                  & " defined in "
                  & To_String (Entity.File_Name)
                  & " line"
                  & Natural'Image (Entity.Line)
                  & " column"
                  & Natural'Image (Entity.Column));
   begin
      Komnenos.UI.Current_UI.Place_Fragment
        (Parent, Offset, Note);
   end Select_Entity;

end Komnenos.Entities.Source;
