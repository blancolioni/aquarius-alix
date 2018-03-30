private with Ada.Containers.Indefinite_Vectors;

package Ack.Classes.Layout is

   procedure Generate_Virtual_Table
     (Unit   : in out Tagatha.Units.Tagatha_Unit;
      Class  : not null access constant Class_Entity_Record'Class);

   procedure Generate_Object_Allocator
     (Unit   : in out Tagatha.Units.Tagatha_Unit;
      Class  : not null access constant Class_Entity_Record'Class);

   type Object_Layout is private;

   procedure Write (Layout : Object_Layout);

   type Virtual_Table_Layout is private;

   procedure Write (Layout : Virtual_Table_Layout);

   function Create_Virtual_Table_Layout
     (Class : not null access constant Class_Entity_Record'Class)
      return Virtual_Table_Layout;

   function Create_Object_Layout
     (Class : not null access constant Class_Entity_Record'Class)
      return Object_Layout;

private

   type Layout_Entry_Type is
     (Property_Value,
      Internal_Offset,
      Table_Link,
      Feature_Address,
      Label_Link);

   type Layout_Entry
     (Entry_Type : Layout_Entry_Type)
   is
      record
         Label : Name_Id := No_Name;
         case Entry_Type is
            when Property_Value =>
               Property : Ack.Features.Constant_Feature_Entity;
            when Internal_Offset =>
               Word_Offset : Integer;
            when Table_Link =>
               Table_Class : Constant_Class_Entity;
            when Feature_Address =>
               Feature     : Ack.Features.Constant_Feature_Entity;
            when Label_Link =>
               Link_Name   : Name_Id;
         end case;
      end record;

   package Layout_Entry_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Layout_Entry);

   type Object_Layout is
      record
         Entries : Layout_Entry_Vectors.Vector;
      end record;

   type Virtual_Table_Layout is new Object_Layout;

end Ack.Classes.Layout;
