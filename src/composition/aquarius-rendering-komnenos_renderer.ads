with Komnenos.Entities;
with Komnenos.Fragments;

package Aquarius.Rendering.Komnenos_Renderer is

   function Fragment_Renderer
     (Target : Komnenos.Fragments.Text_Fragment;
      Entity_Table : access Komnenos.Entities.Entity_Table_Interface'Class)
      return Aquarius_Renderer;

end Aquarius.Rendering.Komnenos_Renderer;
