with Komnenos.Entities;
with Komnenos.Fragments;

package Aquarius.Rendering.Komnenos_Renderer is

   function Fragment_Renderer
     (Target : Komnenos.Fragments.Fragment_Type;
      Entity_Table : access Komnenos.Entities.Entity_Table_Interface'Class)
      return Aquarius_Renderer;

end Aquarius.Rendering.Komnenos_Renderer;
