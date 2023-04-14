private with Ada.Containers.Ordered_Sets;
private with Ada.Containers.Vectors;
private with WL.String_Maps;

with As.Expressions;
with As.Files;

private package As.Environment is

   type Instance is tagged private;
   type Reference is access all Instance'Class;

   function Create return Reference;

   procedure Set_Location
     (This : in out Instance'Class;
      Loc  : Word_32);

   function Get_Location
     (This : Instance'Class)
      return Word_32;

   function Contains
     (This : Instance'Class;
      Name : String)
      return Boolean;

   function Has_Value
     (This : Instance'Class;
      Name : String)
      return Boolean
     with Pre => This.Contains (Name);

   function Get_Value
     (This : Instance'Class;
      Name : String)
      return As.Expressions.Reference
     with Pre => This.Contains (Name) and then This.Has_Value (Name);

   procedure Insert
     (This  : in out Instance'Class;
      Name  : String;
      Value : As.Expressions.Reference)
     with Pre => not This.Contains (Name),
     Post => This.Contains (Name) and then This.Has_Value (Name)
     and then As.Expressions."=" (This.Get_Value (Name), Value);

   procedure Insert
     (This  : in out Instance'Class;
      Name  : String;
      Value : Word_32)
     with Pre => not This.Contains (Name),
     Post => This.Contains (Name) and then This.Has_Value (Name)
     and then This.Get_Value (Name).Has_Static_Value
     and then This.Get_Value (Name).Get_Static_Value = Value;

   procedure Insert
     (This  : in out Instance'Class;
      Name  : String)
     with Pre => not This.Contains (Name),
     Post => This.Contains (Name) and then not This.Has_Value (Name);

   procedure Insert_Local_Label
     (This  : in out Instance'Class;
      Index : Positive);

   function Contains_Local_Label
     (This    : Instance'Class;
      Index   : Positive;
      Forward : Boolean)
      return Boolean;

   function Find_Local_Label
     (This    : Instance'Class;
      Index   : Positive;
      Forward : Boolean)
      return Word_32
     with Pre => This.Contains_Local_Label (Index, Forward);

   procedure Insert_Global
     (This    : in out Instance'Class;
      Context : As.Files.File_Context;
      Name    : String;
      Value   : Word_32);

   function Last_Global
     (This : Instance'Class)
      return Register_Index;

   function Get_Global
     (This : Instance'Class;
      G    : Register_Index)
      return Word_32;

   procedure Get_Global
     (This    : Instance'Class;
      Context : As.Files.File_Context;
      G       : out Register_Index;
      W       : out Word_32);

   procedure Update
     (This  : in out Instance'Class;
      Name  : String;
      Value : As.Expressions.Reference)
     with Pre => This.Contains (Name) and then not This.Has_Value (Name),
     Post => This.Contains (Name) and then This.Has_Value (Name)
     and then As.Expressions."=" (This.Get_Value (Name), Value);

   procedure Update
     (This  : in out Instance'Class;
      Name  : String;
      Value : Word_32)
     with Pre => This.Contains (Name) and then not This.Has_Value (Name);

   procedure Iterate
     (This : Instance'Class;
      Process : not null access
        procedure (Name : String));

private

   type Entry_Record is
      record
         Defined : Boolean := False;
         Value   : As.Expressions.Reference;
      end record;

   package Entry_Maps is
     new WL.String_Maps (Entry_Record);

   type Global_Entry_Record is
      record
         Context  : As.Files.File_Context;
         Register : Register_Index;
         Value    : Word_32;
      end record;

   type Global_Array is
     array (Register_Index range 32 .. 255) of Global_Entry_Record;

   package Local_Label_Sets is
     new Ada.Containers.Ordered_Sets
       (Element_Type => Word_32);

   package Local_Label_Vectors is
     new Ada.Containers.Vectors (Positive, Local_Label_Sets.Set,
                                 Local_Label_Sets."=");

   type Instance is tagged
      record
         Loc         : Word_32 := 0;
         Map         : Entry_Maps.Map;
         Last_Global : Register_Index := 255;
         Globals     : Global_Array;
         Locals      : Local_Label_Vectors.Vector;
      end record;

   function Contains
     (This : Instance'Class;
      Name : String)
      return Boolean
   is (This.Map.Contains (Name));

   function Has_Value
     (This : Instance'Class;
      Name : String)
      return Boolean
   is (This.Map (Name).Defined);

   function Get_Value
     (This : Instance'Class;
      Name : String)
      return As.Expressions.Reference
   is (This.Map (Name).Value);

   function Get_Location
     (This : Instance'Class)
      return Word_32
   is (This.Loc);

   function Last_Global
     (This : Instance'Class)
      return Register_Index
   is (This.Last_Global);

   function Get_Global
     (This : Instance'Class;
      G    : Register_Index)
      return Word_32
   is (This.Globals (G).Value);

end As.Environment;
