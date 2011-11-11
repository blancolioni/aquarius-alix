package body Aquarius.UI.Console is

   type Console_UI_Type is
     new Aquarius_UI with null record;

   overriding
   function Name (Item : Console_UI_Type) return String;

   overriding
   procedure Show_Interactor
     (UI    : access Console_UI_Type;
      Item  : access Aquarius.Interaction.Interactor'Class)
      is null;

   ----------------
   -- Console_UI --
   ----------------

   function Console_UI
     return access Aquarius_UI'Class
   is
   begin
      return new Console_UI_Type;
   end Console_UI;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Item : Console_UI_Type) return String is
      pragma Unreferenced (Item);
   begin
      return "Console UI";
   end Name;

end Aquarius.UI.Console;
