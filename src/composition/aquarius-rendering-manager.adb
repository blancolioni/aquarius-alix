with Aquarius.Rendering.Html;
with Aquarius.Rendering.Text;

package body Aquarius.Rendering.Manager is

   -------------------
   -- Load_Renderer --
   -------------------

   function Renderer (Name : String) return Aquarius_Renderer is
   begin
      if Name = "text" then
         return Aquarius.Rendering.Text.Text_Renderer;
      elsif Name = "html" then
         return Aquarius.Rendering.Html.Html_Renderer;
      else
         return Aquarius.Rendering.Text.Text_Renderer;
      end if;
   end Renderer;

end Aquarius.Rendering.Manager;
