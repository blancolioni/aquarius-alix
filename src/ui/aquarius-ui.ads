with Aquarius.Interaction;
limited with Aquarius.Projects;
with Aquarius.Sections.Layout;

package Aquarius.UI is

   type Root_UI_Type is abstract new Root_Aquarius_Object with private;

   overriding
   function Name (Item : Root_UI_Type) return String;

   procedure Show_Interactor
     (Item : Root_UI_Type;
      Interactor : not null access Aquarius.Interaction.Interactor'Class)
   is null;

   procedure Init (With_UI : not null access Root_UI_Type) is abstract;
   procedure Start (With_UI : in out Root_UI_Type) is abstract;

   procedure Show_Section (On      : in out Root_UI_Type;
                           Section : Aquarius.Sections.Aquarius_Section;
                           Hint_X  : Integer;
                           Hint_Y  : Integer)
   is abstract;

   procedure Update_Message_View (With_UI : Root_UI_Type) is null;

   procedure Show_Project
     (User_Interface : in out Root_UI_Type;
      Project        : not null access
        Aquarius.Projects.Aquarius_Project_Type'Class);

   function Current_Project
     (UI : Root_UI_Type'Class)
      return access Aquarius.Projects.Aquarius_Project_Type'Class;

   type Aquarius_UI is access all Root_UI_Type'Class;

private

   type Root_UI_Type is abstract new Root_Aquarius_Object with
      record
         Project : access Aquarius.Projects.Aquarius_Project_Type'Class;
         Layout  : Aquarius.Sections.Layout.Section_Layout;
      end record;

end Aquarius.UI;
