with Aqua.Execution;
with Aqua.Primitives;
with Aqua.Words;

with Komnenos.Entities.Source.Aquarius_Source;

package body Komnenos.Entities.Aqua_Entities is

   Local_Aqua_Object : Komnenos_Aqua_Object := null;

   procedure Create_Aqua_Primitives;

   function Handle_Define
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   function Handle_Cross_Reference
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   function Handle_Get_Entity
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   ------------------------
   -- Create_Aqua_Object --
   ------------------------

   procedure Create_Aqua_Object
     (Table : access Entity_Table_Interface'Class)
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
      Aqua.Primitives.New_Primitive_Function
        (Name           => "komnenos__define",
         Argument_Count => 4,
         Handler        => Handle_Define'Access);
      Aqua.Primitives.New_Primitive_Function
        (Name           => "komnenos__cross_reference",
         Argument_Count => 3,
         Handler        => Handle_Cross_Reference'Access);
      Aqua.Primitives.New_Primitive_Function
        (Name           => "komnenos__get_entity",
         Argument_Count => 3,
         Handler        => Handle_Get_Entity'Access);
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

   ----------------------------
   -- Handle_Cross_Reference --
   ----------------------------

   function Handle_Cross_Reference
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      use Aquarius.Programs;
      use Komnenos.Entities.Source.Aquarius_Source;
      Aqua_Object : constant Komnenos_Aqua_Object :=
                      Komnenos_Aqua_Object
                        (Context.To_External_Object (Arguments (1)));
   begin

      if Aqua_Object.Table = null then
         return 0;
      end if;

      declare
         use type Aqua.Word;
         Source_Ext  : constant access Aqua.External_Object_Interface'Class :=
                         Context.To_External_Object (Arguments (2));
         Source      : constant Program_Tree := Program_Tree (Source_Ext);
         Ref_Ext     : constant access Aqua.External_Object_Interface'Class :=
                         Context.To_External_Object (Arguments (3));
         Ref         : constant Entity_Reference :=
                         Entity_Reference (Ref_Ext);
      begin
         Add_Cross_Reference
           (Table     => Aqua_Object.Table.all,
            Item      => Ref,
            File_Name => Source.Source_File_Name,
            Line      => Source.Location_Line,
            Column    => Source.Location_Column,
            Ref_Type  => "reference");
      end;

      return 1;
   end Handle_Cross_Reference;

   -------------------
   -- Handle_Define --
   -------------------

   function Handle_Define
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      use Aquarius.Programs;
      use Komnenos.Entities.Source.Aquarius_Source;
      Aqua_Object : constant Komnenos_Aqua_Object :=
                      Komnenos_Aqua_Object
                        (Context.To_External_Object (Arguments (1)));
   begin

      if Aqua_Object.Table = null then
         return 0;
      end if;

      declare
         use type Aqua.Word;
         Parent_Ext  : constant access Aqua.External_Object_Interface'Class :=
                         Context.To_External_Object (Arguments (2));
         Parent      : constant Program_Tree := Program_Tree (Parent_Ext);
         Child_Arg   : constant Aqua.Word := Arguments (3);
         Is_Tree     : constant Boolean :=
                         Aqua.Is_External_Reference (Child_Arg);
         Child_Ext   : constant access Aqua.External_Object_Interface'Class :=
                         (if Is_Tree
                          then Context.To_External_Object (Arguments (3))
                          else null);
         Child_Tree  : constant Program_Tree :=
                         (if Is_Tree
                          then Program_Tree_Type'Class (Child_Ext.all)'Access
                          else null);
         Child       : constant String :=
                         (if Is_Tree
                          then Child_Tree.Text
                          else Context.To_String (Child_Arg));
         Class   : constant String :=
                         Context.To_String (Arguments (4));
         Top_Level   : constant Boolean :=
                         Arguments'Length < 5
                           or else Arguments (5) /= 0;
         Entity  : constant Komnenos.Entities.Entity_Reference :=
                     Create_Aquarius_Source_Entity
                       (Table            => Aqua_Object.Table,
                        Name             => Child,
                        File_Name        => Parent.Source_File_Name,
                        Class            => Class,
                        Line             => Parent.Location_Line,
                        Column           => Parent.Location_Column,
                        Top_Level        => Top_Level,
                        Compilation_Unit => Parent.Program_Root,
                        Entity_Spec      => Parent,
                        Entity_Body      => null);
      begin
         return Context.To_Word (Entity);
      end;
   end Handle_Define;

   -----------------------
   -- Handle_Get_Entity --
   -----------------------

   function Handle_Get_Entity
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      use Aquarius.Programs;
      use Komnenos.Entities.Source.Aquarius_Source;
      Aqua_Object : constant Komnenos_Aqua_Object :=
                      Komnenos_Aqua_Object
                        (Context.To_External_Object (Arguments (1)));
      Entity_Name : constant String :=
                      Context.To_String (Arguments (2));
      Class_Name  : constant String :=
                      (if Arguments'Length >= 3
                       then Context.To_String (Arguments (3))
                       else "");
   begin

      if Aqua_Object.Table = null then
         return 0;
      end if;

      declare
         Entity : constant Entity_Reference :=
                    Aqua_Object.Table.Find
                      (Entity_Name, Class_Name);
      begin
         if Entity = null then
            return 0;
         else
            return Context.To_Word (Entity);
         end if;
      end;

   end Handle_Get_Entity;

end Komnenos.Entities.Aqua_Entities;
