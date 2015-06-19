with Aquarius.Interaction;
limited with Aquarius.Projects;

package Aquarius.UI is

   type Root_UI_Type is interface and Root_Aquarius_Object;

   procedure Show_Interactor
     (Item : Root_UI_Type;
      Interactor : not null access Aquarius.Interaction.Interactor'Class)
   is null;

   procedure Start (With_UI : in out Root_UI_Type) is abstract;

   procedure Update_Message_View (With_UI : Root_UI_Type) is null;

   procedure Show_Project
     (User_Interface : in out Root_UI_Type;
      Project        : not null access
        Aquarius.Projects.Aquarius_Project_Type'Class)
   is null;

   type Aquarius_UI is access all Root_UI_Type'Class;

end Aquarius.UI;
