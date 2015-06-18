with Komnenos.Styles;

package body Komnenos.Configure.Styles is

   ----------------------
   -- Configure_Styles --
   ----------------------

   procedure Configure_Styles (Config : Tropos.Configuration) is
   begin
      for Cfg of Config loop
         declare
            Base_Style : Komnenos.Styles.Komnenos_Style :=
                           Komnenos.Styles.Null_Style;
         begin
            if Cfg.Contains ("base") then
               Base_Style :=
                 Komnenos.Styles.Find_Style
                   (Cfg.Get ("base"));
            end if;

            declare
               Style : constant Komnenos.Styles.Komnenos_Style :=
                         Komnenos.Styles.New_Style
                           (Name           => Cfg.Config_Name,
                            Base           => Base_Style,
                            Foreground     => Cfg.Get ("foreground", ""),
                            Background     => Cfg.Get ("background", ""),
                            Font_Name      => Cfg.Get ("font_name", ""),
                            Font_Size      => Cfg.Get ("font_size", 0),
                            Bold           => Cfg.Get ("bold", False),
                            Italic         => Cfg.Get ("italic", False),
                            Underlined     => Cfg.Get ("underline", False),
                            Strike_Through =>
                              Cfg.Get ("strikethrough", False),
                            Mouse_Cursor   =>
                              Komnenos.Styles.Mouse_Cursor_Type'Value
                                (Cfg.Get ("cursor", "default")));
               pragma Unreferenced (Style);
            begin
               null;
            end;
         end;
      end loop;

   end Configure_Styles;

end Komnenos.Configure.Styles;
