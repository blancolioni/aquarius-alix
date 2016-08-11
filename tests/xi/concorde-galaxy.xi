package Concorde.Galaxy is

   Star_Texture : Texture (Image => "images/galaxy/star.png");
     
   material Star is
      procedure Vertex_Shader
        (Position       : in out Vertex_4;
	     Texture_ST     : in out Texture_Coordinate;
	     Base_Color     : in out Color_4;
	     MVP            : uniform Model_View_Projection_Matrix)
	  is
	  begin
	     Position := MVP * Position;
	  end Vertex_Shader;
	  
      procedure Fragment_Shader
	    (Texture_ST : in Texture_Coordinate;
	     Base_Color : in Color_4;
		 Color      : out Color_4;
		 Tex        : uniform Sampler (Texture_Coordinate, Color_4))
	  is
	  begin
	     Color := Tex.Get (Texture_ST);
		 Color.X := Color.X * Base_Color.X;
		 Color.Y := Color.Y * Base_Color.Y;
		 Color.Z := Color.Z * Base_Color.Z;
	  end Fragment_Shader;
	  
   begin
      technique is
         pass is
            Texture => Star_Texture;
            Vertex_Shader => Vertex_Shader;
            Fragment_Shader => Fragment_Shader;
         end pass;
      end technique;
   end Star;
	  
end Concorde.Galaxy;