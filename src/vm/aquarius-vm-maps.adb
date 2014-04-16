package body Aquarius.VM.Maps is

   --------------
   -- Contains --
   --------------

   function Contains
     (Property : Map_Property_Type'Class;
      Key      : String)
      return Boolean
   is
   begin
      return Property.Map.Contains
        (Ada.Strings.Unbounded.To_Unbounded_String (Key));
   end Contains;

   -------------
   -- Element --
   -------------

   function Element
     (Property : Map_Property_Type'Class;
      Key      : String)
      return VM_Value
   is
   begin
      return Property.Map.Element
        (Ada.Strings.Unbounded.To_Unbounded_String (Key));
   end Element;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Property : in out Map_Property_Type'Class;
      Key      : in     String;
      Value    : in     VM_Value)
   is
   begin
      Property.Map.Insert
        (Ada.Strings.Unbounded.To_Unbounded_String (Key), Value);
   end Insert;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Property : Map_Property_Type'Class;
      Process  : not null access
        procedure (Key : String;
                   Value : VM_Value))
   is
      use VM_Value_Maps;
   begin
      for Position in Property.Map.Iterate loop
         Process (Ada.Strings.Unbounded.To_String (Key (Position)),
                  Element (Position));
      end loop;
   end Iterate;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Item : Map_Property_Type)
      return String
   is
      pragma Unreferenced (Item);
   begin
      return "value-map";
   end Name;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Property : in out Map_Property_Type'Class;
      Key      : in     String;
      Value    : in     VM_Value)
   is
   begin
      Property.Map.Replace
        (Ada.Strings.Unbounded.To_Unbounded_String (Key), Value);
   end Replace;

end Aquarius.VM.Maps;
