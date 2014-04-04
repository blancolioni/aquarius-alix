package body Aquarius.UI is

   ----------
   -- Name --
   ----------

   overriding function Name
     (Item : Root_UI_Type)
      return String
   is
      pragma Unreferenced (Item);
   begin
      return "aquarius-ui";
   end Name;

end Aquarius.UI;
