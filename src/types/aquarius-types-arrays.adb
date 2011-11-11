with Aquarius.Types.Composite;

package body Aquarius.Types.Arrays is

   type Array_Type is new Aquarius.Types.Composite.Root_Composite_Type with
      record
         Array_Index       : access Root_Aquarius_Type'Class;
         Array_Component   : access Root_Aquarius_Type'Class;
      end record;

   overriding
   function Create_Derived_Type (Item : Array_Type)
                                return Aquarius_Type;

   overriding
   function Description (Item : Array_Type) return String;

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   overriding
   function Create_Derived_Type
     (Item : Array_Type)
     return Aquarius_Type
   is
   begin
      return new Array_Type'(Item);
   end Create_Derived_Type;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description (Item : Array_Type) return String is
      pragma Unreferenced (Item);
   begin
      return "an array type";
   end Description;

   ------------------------
   -- Get_Component_Type --
   ------------------------

   function Get_Component_Type (Item : Aquarius_Type) return Aquarius_Type is
   begin
      return Aquarius_Type (Array_Type (Item.all).Array_Component);
   end Get_Component_Type;

   --------------------
   -- Get_Index_Type --
   --------------------

   function Get_Index_Type (Item : Aquarius_Type) return Aquarius_Type is
   begin
      return Aquarius_Type (Array_Type (Item.all).Array_Index);
   end Get_Index_Type;

   -------------------
   -- Is_Array_Type --
   -------------------

   function Is_Array_Type (Item : Aquarius_Type) return Boolean is
   begin
      return Item.all in Array_Type'Class;
   end Is_Array_Type;

   --------------------
   -- New_Array_Type --
   --------------------

   function New_Array_Type
     (Index_Type     : access Root_Aquarius_Type'Class;
      Component_Type : access Root_Aquarius_Type'Class;
      Constrained    : in Boolean)
     return Aquarius_Type
   is
      Result : constant Aquarius_Type :=
        new Array_Type'(Aquarius.Types.Composite.Root_Composite_Type with
                          Array_Index       => Index_Type,
                          Array_Component   => Component_Type);
   begin
      Result.Set_Constrained (Constrained);
      return Result;
   end New_Array_Type;

   --------------------
   -- New_Array_Type --
   --------------------

   function New_Array_Type return Aquarius_Type is
   begin
      return new Array_Type;
   end New_Array_Type;

   ------------------------
   -- Set_Component_Type --
   ------------------------

   procedure Set_Component_Type (For_Array_Type : Aquarius_Type;
                                 Component_Type : Aquarius_Type)
   is
   begin
      Array_Type (For_Array_Type.all).Array_Component := Component_Type;
   end Set_Component_Type;

   --------------------
   -- Set_Index_Type --
   --------------------

   procedure Set_Index_Type (For_Array_Type : access Root_Aquarius_Type'Class;
                             Index_Type     : access Root_Aquarius_Type'Class;
                             Constrained    : in     Boolean)
   is
   begin
      Array_Type (For_Array_Type.all).Array_Index       := Index_Type;
      For_Array_Type.Set_Constrained (Constrained);
   end Set_Index_Type;

end Aquarius.Types.Arrays;
