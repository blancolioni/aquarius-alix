with Aquarius.Interaction;

package Aquarius.UI is

   type Aquarius_UI is abstract new Root_Aquarius_Object with null record;

   procedure Show_Interactor
     (UI    : access Aquarius_UI;
      Item  : access Aquarius.Interaction.Interactor'Class)
      is abstract;

   procedure Update_Message_View (Item : in out Aquarius_UI) is null;

end Aquarius.UI;
