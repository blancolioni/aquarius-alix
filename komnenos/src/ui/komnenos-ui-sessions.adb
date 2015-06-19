with Tropos.Reader;
with Tropos.Writer;

package body Komnenos.UI.Sessions is

   ------------------
   -- Load_Session --
   ------------------

   procedure Load_Session
     (UI   : Komnenos_UI;
      Path : String)
   is
      Session_Config : constant Tropos.Configuration :=
                         Tropos.Reader.Read_Config (Path);
   begin
      UI.From_Config (Session_Config);
      UI.View_Left := Session_Config.Get ("view_left");
      UI.View_Top := Session_Config.Get ("view_top");
      UI.View_Width := Session_Config.Get ("view_width");
      UI.View_Height := Session_Config.Get ("view_height");
   end Load_Session;

   ------------------
   -- Save_Session --
   ------------------

   procedure Save_Session
     (UI   : Komnenos_UI;
      Path : String)
   is
      Config : Tropos.Configuration; -- := UI.To_Config;
   begin
      UI.To_Config (Config);
      Config.Add ("view_left", UI.View_Left);
      Config.Add ("view_top", UI.View_Top);
      Config.Add ("view_width", UI.View_Width);
      Config.Add ("view_height", UI.View_Height);
      Tropos.Writer.Write_Config (Config, Path);
   end Save_Session;

end Komnenos.UI.Sessions;
