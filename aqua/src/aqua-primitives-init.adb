with Ada.Characters.Handling;

with Aqua.Objects;

package body Aqua.Primitives.Init is

   Created_Primitives : Boolean := False;

   function Handle_Contains
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_Get
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_Image
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_Include
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_Set
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_To_Lower
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   -----------------------
   -- Create_Primitives --
   -----------------------

   procedure Create_Primitives is
   begin
      if Created_Primitives then
         return;
      end if;

      New_Primitive ("object__contains", 2, Handle_Contains'Access);
      New_Primitive ("object__get", 2, Handle_Get'Access);
      New_Primitive ("object__image", 1, Handle_Image'Access);
      New_Primitive ("object__include", 2, Handle_Include'Access);
      New_Primitive ("object__set", 3, Handle_Set'Access);
      New_Primitive ("string__to_lower", 1, Handle_To_Lower'Access);

      Created_Primitives := True;

   end Create_Primitives;

   ---------------------
   -- Handle_Contains --
   ---------------------

   function Handle_Contains
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
      Object : constant access Aqua.Objects.Object_Interface'Class :=
                 Aqua.Objects.Object_Interface'Class
                   (Context.To_External_Object (Arguments (1)).all)'Access;
      Name   : constant String :=
                 Context.To_String (Arguments (2));
   begin
      if Object.Has_Property (Name) then
         return To_Integer_Word (1);
      else
         return To_Integer_Word (0);
      end if;
   end Handle_Contains;

   ----------------
   -- Handle_Get --
   ----------------

   function Handle_Get
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
      Object : constant access Aqua.Objects.Object_Interface'Class :=
                 Aqua.Objects.Object_Interface'Class
                   (Context.To_External_Object (Arguments (1)).all)'Access;
      Name   : constant String :=
                 Context.To_String (Arguments (2));
   begin
      if not Object.Has_Property (Name) then
         return 0;
      else
         return Object.Get_Property (Name);
      end if;
   end Handle_Get;

   ------------------
   -- Handle_Image --
   ------------------

   function Handle_Image
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
   begin
      return Context.To_String_Word
        (Context.Show (Arguments (Arguments'First)));
   end Handle_Image;

   --------------------
   -- Handle_Include --
   --------------------

   function Handle_Include
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
      Object : constant access Aqua.Objects.Object_Interface'Class :=
                 Aqua.Objects.Object_Interface'Class
                   (Context.To_External_Object (Arguments (1)).all)'Access;
      Name   : constant String :=
                 Context.To_String (Arguments (2));
   begin
      if not Object.Has_Property (Name) then
         Object.Set_Property (Name, To_Integer_Word (1));
      end if;
      return Arguments (1);
   end Handle_Include;

   ----------------
   -- Handle_Set --
   ----------------

   function Handle_Set
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
      Object : constant access Aqua.Objects.Object_Interface'Class :=
                 Aqua.Objects.Object_Interface'Class
                   (Context.To_External_Object (Arguments (1)).all)'Access;
      Name   : constant String :=
                 Context.To_String (Arguments (2));
      Value  : constant Word := Arguments (3);
   begin
      Object.Set_Property (Name, Value);
      return Arguments (1);
   end Handle_Set;

   ---------------------
   -- Handle_To_Lower --
   ---------------------

   function Handle_To_Lower
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
      S : constant String := Context.To_String (Arguments (1));
      T : constant String := Ada.Characters.Handling.To_Lower (S);
   begin
      return Context.To_String_Word (T);
   end Handle_To_Lower;

end Aqua.Primitives.Init;
