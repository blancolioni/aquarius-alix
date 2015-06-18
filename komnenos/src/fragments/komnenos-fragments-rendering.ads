with Aquarius.Rendering;

package Komnenos.Fragments.Rendering is

   function New_Fragment_Renderer
     (Target : Fragment_Type;
      Entity_Table : access Komnenos.Entities.Entity_Table_Interface'Class)
      return Aquarius.Rendering.Aquarius_Renderer;

end Komnenos.Fragments.Rendering;
