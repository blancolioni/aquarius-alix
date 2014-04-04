with Ada.Text_IO;

package body Aquarius.Transformers.Action_Script is

   type Action_Script_Transformer is
     new Root_Transformer_Type with null record;

   overriding procedure On_Script_Line
     (Transformer : Action_Script_Transformer;
      Line        : String);

   overriding procedure On_Script_Inline
     (Transformer : Action_Script_Transformer;
      Line        : String);

   overriding procedure On_Text
     (Transformer : Action_Script_Transformer;
      Text        : String);

   overriding procedure On_Text_Line
     (Transformer : Action_Script_Transformer;
      Text        : String);

   ---------------------
   -- Get_Transformer --
   ---------------------

   function Get_Transformer return Root_Transformer_Type'Class is
      Result : Action_Script_Transformer;
   begin
      return Result;
   end Get_Transformer;

   ----------------------
   -- On_Script_Inline --
   ----------------------

   overriding procedure On_Script_Inline
     (Transformer : Action_Script_Transformer;
      Line        : String)
   is
      pragma Unreferenced (Transformer);
   begin
      Ada.Text_IO.Put_Line ("Put (" & Line & ");");
   end On_Script_Inline;

   --------------------
   -- On_Script_Line --
   --------------------

   overriding procedure On_Script_Line
     (Transformer : Action_Script_Transformer;
      Line        : String)
   is
      pragma Unreferenced (Transformer);
   begin
      Ada.Text_IO.Put_Line (Line);
   end On_Script_Line;

   -------------
   -- On_Text --
   -------------

   overriding procedure On_Text
     (Transformer : Action_Script_Transformer;
      Text        : String)
   is
      pragma Unreferenced (Transformer);
   begin
      Ada.Text_IO.Put_Line
        ("Put (""" & Text & """);");
   end On_Text;

   ------------------
   -- On_Text_Line --
   ------------------

   overriding procedure On_Text_Line
     (Transformer : Action_Script_Transformer;
      Text        : String)
   is
      pragma Unreferenced (Transformer);
   begin
      if Text = "" then
         Ada.Text_IO.Put_Line
           ("New_Line;");
      else
         Ada.Text_IO.Put_Line
           ("Put_Line (""" & Text & """);");
      end if;
   end On_Text_Line;

begin
   Register ("actionscript",
             Aquarius.Transformers.Action_Script.Get_Transformer);
end Aquarius.Transformers.Action_Script;
