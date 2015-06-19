with Aqua.Primitives;
with Aqua.Words;

package body Komnenos.Entities.Aqua_Entities is

   Local_Aqua_Object : Komnenos_Aqua_Object := null;

   procedure Create_Aqua_Primitives;

   ------------------------
   -- Create_Aqua_Object --
   ------------------------

   procedure Create_Aqua_Object
     (Table : not null access Entity_Table_Interface'Class)
   is
   begin
      Local_Aqua_Object := new Root_Aqua_Object;
      Local_Aqua_Object.Table := Table;
      Create_Aqua_Primitives;
   end Create_Aqua_Object;

   ----------------------------
   -- Create_Aqua_Primitives --
   ----------------------------

   procedure Create_Aqua_Primitives is
   begin
      null;
   end Create_Aqua_Primitives;

   ---------------------
   -- Get_Aqua_Object --
   ---------------------

   function Get_Aqua_Object
      return Komnenos_Aqua_Object
   is
   begin
      return Local_Aqua_Object;
   end Get_Aqua_Object;

   ------------------
   -- Get_Property --
   ------------------

   overriding function Get_Property
     (Object : in out Root_Aqua_Object;
      Name   : in String)
      return Aqua.Word
   is
      use Aqua;
      Object_Primitive_Name : constant String :=
                                "komnenos__" & Name;
      Object_Primitive      : constant Subroutine_Reference :=
                                Aqua.Primitives.Get_Primitive
                                  (Object_Primitive_Name);
      Result                : Word;
   begin
      if Object_Primitive /= 0 then
         Result := Aqua.Words.To_Subroutine_Word (Object_Primitive);
      else
         Result := Aqua.Objects.Root_Object_Type (Object).Get_Property (Name);
      end if;
      return Result;
   end Get_Property;

end Komnenos.Entities.Aqua_Entities;
