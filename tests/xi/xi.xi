package Xi is

   Default : Texture (Image => "images/textures/default.png");
      
   generic
      Image_Path : String;
   material Basic_Texture_Map
      with Image => Image_Path;
      
   generic
       Color : RGBA;
   material Solid_Color
      with Color => Color;
      
   material Default is new Solid_Color (Color => (1, 1, 1, 1));
   
   generic
      Base_Color : RGBA;
   material Transparent_Color
      with Scene_Blend => Alpha,
           Color       => RGBA (Base_Color.RGB, 1.0),
           Alpha       => Base_Color.A;
           
   material Blue
      with Lighting,
           Ambient  => (0.3, 0.3, 0.3, 1.0),
           Diffuse  => (0.1, 0.2, 0.7, 1.0),
           Emissive => (0.0, 0.0, 0.0, 1.0);

   generic
      Base_Texture    : Texture;
      Blend_Texture   : Texture;
      Alpha_Texture   : Texture;
   material Texture_Blend is
   begin
      Texture_Unit with
         Texture => Base_Texture;
      Texture_Unit with
         Texture => Alpha_Texture,
         Color_Operation => Alpha_Blend (Source_Alpha, Source_RGB);
      Texture_Unit with
         Texture => Blend_Texture,
         Color_Operation => Alpha_Blend (Source_Texture, Source_Current);
   end Texture_Blend;
    
end Xi;
