package Aquarius.Configuration is

   Configuration_Error : exception;

   type Cursor is private;

   procedure Load_Configuration;

   function Get_Grammar_Path (Grammar_Name : String) return String;
   function Get_Library_Path return String;

   function Get_Cursor (Path : String) return Cursor;
   function Has_Element (Position : Cursor) return Boolean;
   function Child_Count (Position : Cursor) return Natural;
   function Get_Child   (Position : Cursor;
                         Index    : Positive)
                        return Cursor;
   function Get_Name    (Position : Cursor) return String;

   function Get_Value (Position      : Cursor;
                       Value_Name    : String;
                       Default_Value : String := "")
                      return String;

   function Get_Value (Position      : Cursor;
                       Value_Name    : String;
                       Default_Value : Boolean := False)
                      return Boolean;

   function Get_Value (Position      : Cursor;
                       Value_Name    : String;
                       Default_Value : Integer := 0)
                      return Integer;

   procedure Set_Value (Position   : Cursor;
                        Value_Name : String;
                        Value      : String);

   procedure Set_Value (Position   : Cursor;
                        Value_Name : String;
                        Value      : Integer);

   procedure Set_Value (Position   : Cursor;
                        Value_Name : String;
                        Value      : Boolean);

   procedure Error (Message : String);
   procedure Warning (Message : String);

private

   type Config_Item_Type is (Nested_Item, Value_Item);

   type Config_Item_Record is tagged;

   type Cursor is access all Config_Item_Record'Class;

end Aquarius.Configuration;
