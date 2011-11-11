with Aquarius.Rendering.Html;
with Aquarius.Rendering.Text;

package body Aquarius.Rendering.Manager is

   -------------------
   -- Load_Renderer --
   -------------------

   function Load_Renderer (Name : String) return Aquarius_Renderer is
   begin
      if Name = "text" then
         return Aquarius.Rendering.Text.New_Text_Renderer;
      elsif Name = "html" then
         return Aquarius.Rendering.Html.New_Html_Renderer;
      else
         return Aquarius.Rendering.Text.New_Text_Renderer;
      end if;
   end Load_Renderer;

end Aquarius.Rendering.Manager;
