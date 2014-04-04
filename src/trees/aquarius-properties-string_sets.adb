package body Aquarius.Properties.String_Sets is

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Property : in out String_Set_Property_Type'Class;
      Value    : in     String)
   is
   begin
      Property.Set.Insert (Value);
   end Insert;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Property : String_Set_Property_Type'Class;
      Process  : not null access
        procedure (Value : String))
   is
   begin
      for V of Property.Set loop
         Process (V);
      end loop;
   end Iterate;

   ------------
   -- Member --
   ------------

   function Member
     (Property : String_Set_Property_Type'Class;
      Value    : String)
      return Boolean
   is
   begin
      return Property.Set.Contains (Value);
   end Member;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Item : String_Set_Property_Type)
      return String
   is
      pragma Unreferenced (Item);
   begin
      return "string_set";
   end Name;

end Aquarius.Properties.String_Sets;
