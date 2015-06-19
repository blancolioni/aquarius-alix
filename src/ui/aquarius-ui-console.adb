package body Aquarius.UI.Console is

   type Console_UI_Type is
     new Root_UI_Type with null record;

   overriding
   function Name (Item : Console_UI_Type) return String;

   overriding
   procedure Show_Interactor
     (UI    : Console_UI_Type;
      Item  : not null access Aquarius.Interaction.Interactor'Class)
      is null;

   overriding
   procedure Init (With_UI : not null access Console_UI_Type) is null;

   overriding
   procedure Start (With_UI : in out Console_UI_Type) is null;

   ----------------
   -- Console_UI --
   ----------------

   function Console_UI return Aquarius_UI is
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
