with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

--  with Aqua.IO;
with Aqua.Objects.Arrays;
with Aqua.Words;

package body Aqua.Primitives.Init is

   Created_Primitives : Boolean := False;

   Local_Object_Class : Primitive_Object_Access;

   Current_Output : Ada.Text_IO.File_Type;
   Output_Redirected : Boolean := False;

   type String_Access is access all String;

   type Method_Record is
      record
         Name : String_Access;
         Impl : Subroutine_Reference;
      end record;

   type Array_Of_Methods is
     array (Positive range <>) of Method_Record;

   procedure New_Class
     (Name : String;
      Methods : Array_Of_Methods);

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

   function Handle_Load_Object
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_Report_State
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_Set
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_Object_New
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_Array_New
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_Array_Append
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_To_Integer
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_To_Lower
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_String_Replace
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_String_Slice
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_Put
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_Put_Line
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_New_Line
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word;

   function Handle_Set_Output
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

      New_Primitive_Function ("object__contains", 2, Handle_Contains'Access);
      New_Primitive_Function ("object__get", 2, Handle_Get'Access);
      New_Primitive_Function ("object__image", 1, Handle_Image'Access);
      New_Primitive_Function ("object__include", 2, Handle_Include'Access);
      New_Primitive_Function ("object__set", 3, Handle_Set'Access);
      New_Primitive_Function ("object__new", 0, Handle_Object_New'Access);

      New_Primitive_Function ("array__new", 0, Handle_Array_New'Access);
      New_Primitive_Function ("array__image", 0, Handle_Image'Access);
      New_Primitive_Function ("array__append", 0, Handle_Array_Append'Access);

      New_Primitive_Function ("string__to_lower", 1, Handle_To_Lower'Access);
      New_Primitive_Function ("string__to_integer", 1,
                              Handle_To_Integer'Access);
      New_Primitive_Function ("string__replace", 3,
                              Handle_String_Replace'Access);
      New_Primitive_Function ("string__slice", 3,
                              Handle_String_Slice'Access);

      New_Primitive_Function ("aqua__report_state", 0,
                              Handle_Report_State'Access);
      New_Primitive_Function ("aqua__load_object", 2,
                              Handle_Load_Object'Access);

      New_Primitive_Function ("io__put", 1,
                              Handle_Put'Access);
      New_Primitive_Function ("io__put_line", 1,
                              Handle_Put_Line'Access);
      New_Primitive_Function ("io__new_line", 0,
                              Handle_New_Line'Access);
      New_Primitive_Function ("io__set_output", 1,
                              Handle_Set_Output'Access);

      declare
         Object_Class : Aqua.Objects.Root_Object_Type;
      begin
         Object_Class.Set_Property
           ("new",
            Aqua.Words.To_Subroutine_Word
              (Get_Primitive ("object__new")));

         Local_Object_Class :=
           new Aqua.Objects.Root_Object_Type'(Object_Class);
         New_Primitive_Object ("map", Local_Object_Class);
      end;

      New_Class ("array",
                 ((new String'("new"), Get_Primitive ("array__new")),
                  (new String'("append"), Get_Primitive ("array__append"))));

      New_Class ("aqua",
                 ((new String'("report_state"),
                  Get_Primitive ("aqua__report_state")),
                  (new String'("load_object"),
                   Get_Primitive ("aqua__load_object"))));

      New_Class ("io",
                 ((new String'("put"), Get_Primitive ("io__put")),
                  (new String'("new_line"), Get_Primitive ("io__new_line")),
                  (new String'("put_line"), Get_Primitive ("io__put_line")),
                  (new String'("set_output"),
                   Get_Primitive ("io__set_output"))));

      Created_Primitives := True;

   end Create_Primitives;

   -------------------------
   -- Handle_Array_Append --
   -------------------------

   function Handle_Array_Append
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
      Arr : constant access Aqua.Objects.Arrays.Root_Array_Type'Class :=
                 Aqua.Objects.Arrays.Root_Array_Type'Class
                   (Context.To_External_Object (Arguments (1)).all)'Access;
      Value  : constant Word := Arguments (2);
   begin
      Arr.Append (Value);
      return Arguments (1);
   end Handle_Array_Append;

   ----------------------
   -- Handle_Array_New --
   ----------------------

   function Handle_Array_New
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
      pragma Unreferenced (Arguments);
      Item : constant Primitive_Object_Access :=
               new Aqua.Objects.Arrays.Root_Array_Type;
   begin
      return Context.To_Word (Item);
   end Handle_Array_New;

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

   ------------------------
   -- Handle_Load_Object --
   ------------------------

   function Handle_Load_Object
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
      File_Name : constant String := Context.To_String (Arguments (2));
      Result    : constant access External_Object_Interface'Class :=
                 Context.Loader.Load_Object (File_Name);
   begin
      return Context.To_Word (Result);
   end Handle_Load_Object;

   ---------------------
   -- Handle_New_Line --
   ---------------------

   function Handle_New_Line
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
      pragma Unreferenced (Context);
      pragma Unreferenced (Arguments);
   begin
      if Output_Redirected then
         Ada.Text_IO.New_Line (Current_Output);
      else
         Ada.Text_IO.New_Line;
      end if;
      return 1;
   end Handle_New_Line;

   -----------------------
   -- Handle_Object_New --
   -----------------------

   function Handle_Object_New
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
      pragma Unreferenced (Arguments);
      Item : constant Primitive_Object_Access :=
               new Aqua.Objects.Root_Object_Type;
   begin
      return Context.To_Word (Item);
   end Handle_Object_New;

   ----------------
   -- Handle_Put --
   ----------------

   function Handle_Put
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
      Text : constant String := Context.To_String (Arguments (2));
   begin
      if Output_Redirected then
         Ada.Text_IO.Put (Current_Output, Text);
      else
         Ada.Text_IO.Put (Text);
      end if;
      return 1;
   end Handle_Put;

   ---------------------
   -- Handle_Put_Line --
   ---------------------

   function Handle_Put_Line
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
      Text : constant String := Context.To_String (Arguments (2));
   begin
      if Output_Redirected then
         Ada.Text_IO.Put_Line (Current_Output, Text);
      else
         Ada.Text_IO.Put_Line (Text);
      end if;
      return 1;
   end Handle_Put_Line;

   -------------------------
   -- Handle_Report_State --
   -------------------------

   function Handle_Report_State
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
      pragma Unreferenced (Arguments);
   begin
      Context.Report;
      return 0;
   end Handle_Report_State;

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

   -----------------------
   -- Handle_Set_Output --
   -----------------------

   function Handle_Set_Output
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
      Name : constant String := Context.To_String (Arguments (2));
   begin
      if Output_Redirected then
         Output_Redirected := False;
         Ada.Text_IO.Close (Current_Output);
      end if;
      if Name = "" then
         return 1;
      else
         begin
            Ada.Text_IO.Create (Current_Output, Ada.Text_IO.Out_File,
                                Ada.Directories.Simple_Name (Name));
--                                  Ada.Directories.Compose
--                                    (Aqua.IO.Current_IO_Path, Name));
            Output_Redirected := True;
            return 1;
         exception
            when others =>
               return 0;
         end;
      end if;
   end Handle_Set_Output;

   ---------------------------
   -- Handle_String_Replace --
   ---------------------------

   function Handle_String_Replace
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
      S      : constant String := Context.To_String (Arguments (1));
      X      : constant String := Context.To_String (Arguments (2));
      Y      : constant String := Context.To_String (Arguments (3));
      Index  : Positive := S'First;
   begin
      while Index <= S'Length - X'Length loop
         if S (Index .. Index + X'Length - 1) = X then
            Result := Result & Y;
            Index := Index + X'Length;
         else
            Result := Result & S (Index);
            Index := Index + 1;
         end if;
      end loop;

      Result := Result & S (Index .. S'Last);

      return Context.To_String_Word (To_String (Result));
   end Handle_String_Replace;

   -------------------------
   -- Handle_String_Slice --
   -------------------------

   function Handle_String_Slice
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
      S      : constant String := Context.To_String (Arguments (1));
      X      : constant Natural :=
                 Natural'Max (Natural (Arguments (2)), S'First);
      Y      : constant Natural :=
                 Natural'Min (X + Natural (Arguments (3)) - 1, S'Last);
   begin
      return Context.To_String_Word (S (X .. Y));
   end Handle_String_Slice;

   -----------------------
   -- Handle_To_Integer --
   -----------------------

   function Handle_To_Integer
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is
      S : constant String := Context.To_String (Arguments (1));
      T : constant Aqua_Integer :=
            Aqua_Integer'Value (S);
   begin
      return To_Integer_Word (T);
   end Handle_To_Integer;

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

   ---------------
   -- New_Class --
   ---------------

   procedure New_Class
     (Name : String;
      Methods : Array_Of_Methods)
   is
      Class : Aqua.Objects.Root_Object_Type;
   begin
      for Method of Methods loop
         Class.Set_Property (Method.Name.all,
                             Aqua.Words.To_Subroutine_Word (Method.Impl));
      end loop;

      declare
         Class_Access : constant Primitive_Object_Access :=
                          new Aqua.Objects.Root_Object_Type'(Class);
      begin
         New_Primitive_Object (Name, Class_Access);
      end;
   end New_Class;

end Aqua.Primitives.Init;
