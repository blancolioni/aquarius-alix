package body Aquarius.UI is

   ---------------------
   -- Current_Project --
   ---------------------

   function Current_Project
     (UI : Root_UI_Type'Class)
      return access Aquarius.Projects.Aquarius_Project_Type'Class
   is
   begin
      return UI.Project;
   end Current_Project;

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

   ------------------
   -- Show_Project --
   ------------------

   procedure Show_Project
     (User_Interface : in out Root_UI_Type;
      Project        : not null access
        Aquarius.Projects.Aquarius_Project_Type'Class)
   is
   begin
      User_Interface.Project := Project;
   end Show_Project;

end Aquarius.UI;
