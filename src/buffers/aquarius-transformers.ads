package Aquarius.Transformers is

   type Root_Transformer_Type is abstract tagged private;

   procedure On_Script_Line
     (Transformer : Root_Transformer_Type;
      Line        : String)
   is abstract;

   procedure On_Script_Inline
     (Transformer : Root_Transformer_Type;
      Line        : String)
   is abstract;

   procedure On_Text
     (Transformer : Root_Transformer_Type;
      Text        : String)
   is abstract;

   procedure On_Text_Line
     (Transformer : Root_Transformer_Type;
      Text        : String)
   is abstract;

   function Get_Transformer
     (Name : String)
      return Root_Transformer_Type'Class;

   procedure Execute_Transformer
     (Transformer      : Root_Transformer_Type'Class;
      Source_Path      : String;
      Destination_Path : String);

private

   procedure Register
     (Name        : String;
      Transformer : Root_Transformer_Type'Class);

   type Root_Transformer_Type is abstract tagged null record;

end Aquarius.Transformers;
