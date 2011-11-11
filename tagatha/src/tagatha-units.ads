with Ada.Strings.Unbounded;

private with Tagatha.Labels;

with Tagatha.Commands;
with Tagatha.Operands;

package Tagatha.Units is

   --  note: all stack offsets are in _words_

   type Tagatha_Unit is tagged private;

   type Tagatha_Unit_Access is access all Tagatha_Unit'Class;

   procedure Create_Unit (New_Unit       : in out Tagatha_Unit;
                          Name           : in     String;
                          Source_File    : in     String;
                          Argument_Words : in     Natural;
                          Frame_Words    : in     Natural;
                          Global         : in     Boolean := True);

   procedure Finish_Unit (Unit : in out Tagatha_Unit);

   procedure Optimise (Unit : in out Tagatha_Unit);

   procedure Write (Unit        : in     Tagatha_Unit;
                    Target_Name : in String);

   procedure Segment (Unit  : in out Tagatha_Unit;
                      Seg   : in     Tagatha_Segment);

   procedure Label (Unit     : in out Tagatha_Unit;
                    Name     : in     String;
                    Export   : in     Boolean := False);

   procedure Next_Label (Unit   : in out Tagatha_Unit;
                         Index  :    out Positive);

   procedure Label (Unit   : in out Tagatha_Unit;
                    Index  : in     Positive);

   procedure Command (Unit    : in out Tagatha_Unit;
                      Command : in     Tagatha.Commands.Tagatha_Command);

   procedure Data (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Integer;
                   Size    : in     Tagatha_Size     := Default_Integer_Size);

   procedure Data (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Integer_Array;
                   Size    : in     Tagatha_Size     := Default_Integer_Size);

   procedure Data (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Floating_Point_Array);

   procedure Data (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Floating_Point);

   procedure Ascii_String (Unit  : in out Tagatha_Unit;
                           Value : in     String);

   procedure Asciz_String (Unit  : in out Tagatha_Unit;
                           Value : in     String);

   procedure Push (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Integer;
                   Size    : in     Tagatha_Size     := Default_Integer_Size);

   procedure Push (Unit  : in out Tagatha_Unit;
                   Value : in     Tagatha_Floating_Point);

   procedure Push_Label (Unit    : in out Tagatha_Unit;
                         Label_Name : String;
                         Size       : Tagatha_Size := Default_Integer_Size);

   procedure Push_Local (Unit    : in out Tagatha_Unit;
                         Offset  : in     Local_Offset;
                         Size    : Tagatha_Size := Default_Integer_Size);

   procedure Push_Argument (Unit    : in out Tagatha_Unit;
                            Offset  : in     Argument_Offset;
                            Size    : Tagatha_Size := Default_Integer_Size);

   --  procedure Push_Label_Address (Unit       : in out Tagatha_Unit;
   --                                Label_Name : String);

   --  procedure Push_Local_Address (Unit    : in out Tagatha_Unit;
   --                                Offset  : in     Local_Offset);

   --  procedure Push_Argument_Address (Unit    : in out Tagatha_Unit;
   --                                   Offset  : in     Argument_Offset);

   procedure Pop_Label (Unit       : in out Tagatha_Unit;
                        Label_Name : String;
                        Size       : Tagatha_Size := Default_Integer_Size);

   procedure Pop_Local (Unit    : in out Tagatha_Unit;
                        Offset  : in     Local_Offset;
                        Size    : in     Tagatha_Size :=
                          Default_Integer_Size);

   procedure Pop_Argument
     (Unit    : in out Tagatha_Unit;
      Offset  : in     Argument_Offset;
      Size    : in     Tagatha_Size := Default_Integer_Size);

   procedure Pop_Result
     (Unit    : in out Tagatha_Unit;
      Size    : in     Tagatha_Size := Default_Integer_Size);

   procedure Dereference (Unit : in out Tagatha_Unit;
                          Size : in     Tagatha_Size := Default_Integer_Size);

   procedure Operate (Unit   : in out Tagatha_Unit;
                      Op     : Tagatha_Operator;
                      Size   : Tagatha_Size       := Default_Integer_Size);

   procedure Call (Unit   : in out Tagatha_Unit;
                   Target : in     String);

   procedure Jump (Unit      : in out Tagatha_Unit;
                   Target    : in     Integer;
                   Condition : in     Tagatha_Condition := C_Always);

   procedure Loop_Around (Unit         : in out Tagatha_Unit;
                          Label_Name   : in     String;
                          Loop_Count   : in     Local_Offset;
                          Loop_Index   : in     Local_Offset);

   function External_Name (Unit : Tagatha_Unit) return String;
   function File_System_Name (Unit : Tagatha_Unit) return String;

   procedure Pop_Operand (Unit      : in out Tagatha_Unit;
                          Op        : in     Operands.Tagatha_Operand;
                          Size      : in     Tagatha_Size);

   procedure Push_Operand (Unit      : in out Tagatha_Unit;
                           Op        : in     Operands.Tagatha_Operand;
                           Size      : in     Tagatha_Size);

private

   type Tagatha_Unit_Record;

   type Tagatha_Unit_Record_Access is
     access all Tagatha_Unit_Record;

   type Segment_Length_Array is array (Tagatha_Segment) of Natural;

   type Tagatha_Unit is tagged
      record
         Name               : Ada.Strings.Unbounded.Unbounded_String;
         Source_File        : Ada.Strings.Unbounded.Unbounded_String;
         Current_Segment    : Tagatha_Segment        := Executable;
         Next_Address       : Segment_Length_Array   := (others => 1);
         Argument_Words     : Natural;
         Frame_Words        : Natural;
         Last_Label         : Tagatha.Labels.Tagatha_Label;
         Global             : Boolean := True;
         Unit               : Tagatha_Unit_Record_Access;
      end record;

end Tagatha.Units;
