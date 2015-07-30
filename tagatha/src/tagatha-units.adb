with Ada.Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Strings.Maps;
with Ada.Text_IO;

with Tagatha.Code;
with Tagatha.Constants;
with Tagatha.File_Assembly;
with Tagatha.Registry;
with Tagatha.Transfers;

--  with Tagatha.Units.Optimisation;

package body Tagatha.Units is

   procedure Write (Unit   : in     Tagatha_Unit;
                    Target : in out Tagatha.Code.Translator'Class);

   package Command_Vector is
     new Ada.Containers.Vectors (Positive,
                                 Tagatha.Commands.Tagatha_Command,
                                 Tagatha.Commands."=");

   type Tagatha_Data_Type is
     (Integer_Data, Floating_Point_Data,
      String_Data, Label_Data);

   type Tagatha_Data (Data_Type : Tagatha_Data_Type := Integer_Data) is
      record
         Label : Tagatha.Labels.Tagatha_Label;
         Size : Tagatha_Size;
         case Data_Type is
            when Integer_Data =>
               Integer_Value        : Tagatha_Integer;
            when Floating_Point_Data =>
               Floating_Point_Value : Tagatha_Floating_Point;
            when Label_Data =>
               Label_Value          : Tagatha.Labels.Tagatha_Label;
            when String_Data =>
               String_Value         : String (1 .. 16);
         end case;
      end record;

   package Data_Vector is
     new Ada.Containers.Vectors (Positive, Tagatha_Data);

   type Array_Of_Transfers_Access is
     access Tagatha.Transfers.Array_Of_Transfers;

   type Tagatha_Subprogram_Record is
      record
         Name               : Ada.Strings.Unbounded.Unbounded_String;
         Current_Segment    : Tagatha_Segment        := Executable;
         Next_Address       : Segment_Length_Array   := (others => 1);
         Argument_Words     : Natural;
         Frame_Words        : Natural;
         Result_Words       : Natural;
         Last_Label         : Tagatha.Labels.Tagatha_Label;
         Global             : Boolean := True;
         Executable_Segment : Command_Vector.Vector;
         Read_Only_Segment  : Data_Vector.Vector;
         Read_Write_Segment : Data_Vector.Vector;
         Transfers          : Array_Of_Transfers_Access;
      end record;

   function Subprogram_Name
     (Unit       : Tagatha_Unit'Class;
      Subprogram : Tagatha_Subprogram_Record_Access)
      return String
   is (Ada.Strings.Unbounded.To_String (Subprogram.Name));

   procedure Append (To_Unit : in out Tagatha_Unit;
                     Command : in     Tagatha.Commands.Tagatha_Command);

   procedure Increment_Address (For_Unit    : in out Tagatha_Unit;
                                By          : in     Positive         := 1);

   procedure Increment_Address (For_Unit    : in out Tagatha_Unit;
                                For_Segment : in     Tagatha_Segment;
                                By          : in     Positive         := 1);

   ------------
   -- Append --
   ------------

   procedure Append (To_Unit : in out Tagatha_Unit;
                     Command : in     Tagatha.Commands.Tagatha_Command)
   is
   begin
      Tagatha.Commands.Set_Label (Command, To_Unit.Last_Label (Executable));
      To_Unit.Last_Label (Executable) := Tagatha.Labels.No_Label;
      Command_Vector.Append (To_Unit.Current_Sub.Executable_Segment, Command);
      Increment_Address (To_Unit, Executable);
   end Append;

   ------------------
   -- Ascii_String --
   ------------------

   procedure Ascii_String (Unit  : in out Tagatha_Unit;
                           Value : in     String)
   is
      Start : Positive := Value'First;
   begin
      while Start <= Value'Last loop
         declare
            Data : Tagatha_Data :=
                     (String_Data,
                      Unit.Last_Label (Unit.Current_Segment),
                      Size_8,
                      (others => ' '));
            Last : Positive     := Start + Data.String_Value'Length;
         begin
            Unit.Last_Label (Unit.Current_Segment) :=
              Tagatha.Labels.No_Label;
            if Last > Value'Last then
               Last := Value'Last;
            end if;
            Data.String_Value (1 .. Last - Start + 1) := Value;
            Unit.Current_Sub.Read_Only_Segment.Append (Data);
            Increment_Address (Unit, Read_Only);
            Start := Start + Data.String_Value'Length;
         end;
      end loop;
   end Ascii_String;

   ------------------
   -- Asciz_String --
   ------------------

   procedure Asciz_String (Unit  : in out Tagatha_Unit;
                           Value : in     String)
   is
   begin
      Ascii_String (Unit, Value & Character'Val (0));
   end Asciz_String;

   -------------------
   -- Begin_Routine --
   -------------------

   procedure Begin_Routine
     (Unit           : in out Tagatha_Unit;
      Name           : in     String;
      Argument_Words : in     Natural;
      Frame_Words    : in     Natural;
      Result_Words   : in     Natural;
      Global         : in     Boolean)
   is
      use Ada.Strings.Unbounded;
   begin
      Unit.Current_Sub := new Tagatha_Subprogram_Record;
      Unit.Subprograms.Append (Unit.Current_Sub);
      Unit.Current_Sub.Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Unit.Current_Sub.Argument_Words := Argument_Words;
      Unit.Current_Sub.Frame_Words := Frame_Words;
      Unit.Current_Sub.Result_Words := Result_Words;
      Unit.Current_Sub.Global := Global;
   end Begin_Routine;

   ----------
   -- Call --
   ----------

   procedure Call (Unit   : in out Tagatha_Unit;
                   Target : in     String)
   is
      Label : Tagatha.Labels.Tagatha_Label;
   begin
      Tagatha.Labels.Reference_Label (Unit.Labels, Label,
                                      Target);
      Append (Unit, Commands.Call (Label));
   end Call;

   -------------
   -- Command --
   -------------

   procedure Command (Unit    : in out Tagatha_Unit;
                      Command : in     Tagatha.Commands.Tagatha_Command)
   is
   begin
      Append (Unit, Command);
   end Command;

   -----------------
   -- Create_Unit --
   -----------------

   procedure Create_Unit
     (New_Unit       : in out Tagatha_Unit;
      Name           : in     String;
      Source_File    : in     String)
   is
      use Ada.Strings.Unbounded;
      Unit : Tagatha_Unit;
   begin
      Unit.Name := To_Unbounded_String (Name);
      Unit.Source_File := To_Unbounded_String (Source_File);
      New_Unit := Unit;
   end Create_Unit;

   ----------
   -- Data --
   ----------

   procedure Data (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Integer;
                   Size    : in     Tagatha_Size     := Default_Integer_Size)
   is
      New_Value : constant Tagatha_Data :=
                    (Integer_Data, Unit.Last_Label (Unit.Current_Segment),
                     Size, Value);
   begin
      Unit.Last_Label (Unit.Current_Segment) := Tagatha.Labels.No_Label;
      case Unit.Current_Segment is
         when Executable | Read_Only =>
            Unit.Current_Sub.Read_Only_Segment.Append (New_Value);
         when Read_Write =>
            Unit.Current_Sub.Read_Write_Segment.Append (New_Value);
      end case;
      Increment_Address (Unit);
   end Data;

   ----------
   -- Data --
   ----------

   procedure Data
     (Unit       : in out Tagatha_Unit;
      Label_Name : in     String;
      Size       : in     Tagatha_Size     := Default_Integer_Size)
   is
      Label : Tagatha.Labels.Tagatha_Label;
      New_Value : Tagatha_Data;
   begin
      Tagatha.Labels.Reference_Label (Unit.Labels, Label,
                                      Label_Name, Import => True);
      New_Value :=
        (Label_Data, Unit.Last_Label (Unit.Current_Segment),
         Size, Label);
      Unit.Last_Label (Unit.Current_Segment) := Tagatha.Labels.No_Label;
      case Unit.Current_Segment is
         when Executable | Read_Only =>
            Unit.Current_Sub.Read_Only_Segment.Append (New_Value);
         when Read_Write =>
            Unit.Current_Sub.Read_Write_Segment.Append (New_Value);
      end case;
      Increment_Address (Unit);
   end Data;

   ----------
   -- Data --
   ----------

   procedure Data (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Integer_Array;
                   Size    : in     Tagatha_Size     := Default_Integer_Size)
   is
   begin
      for I in Value'Range loop
         Data (Unit, Value (I), Size);
      end loop;
   end Data;

   ----------
   -- Data --
   ----------

   procedure Data (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Floating_Point)
   is
      New_Value : constant Tagatha_Data :=
                    (Floating_Point_Data,
                     Unit.Last_Label (Unit.Current_Segment),
                     Default_Size, Value);
   begin
      Unit.Last_Label (Unit.Current_Segment) := Tagatha.Labels.No_Label;
      case Unit.Current_Segment is
      when Executable | Read_Only =>
         Unit.Current_Sub.Read_Only_Segment.Append (New_Value);
      when Read_Write =>
         Unit.Current_Sub.Read_Write_Segment.Append (New_Value);
      end case;
      Increment_Address (Unit);
   end Data;

   ----------
   -- Data --
   ----------

   procedure Data (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Floating_Point_Array)
   is
   begin
      for I in Value'Range loop
         Data (Unit, Value (I));
      end loop;
   end Data;

   -----------------
   -- Dereference --
   -----------------

   procedure Dereference (Unit : in out Tagatha_Unit;
                          Size : in     Tagatha_Size := Default_Integer_Size)
   is
   begin
      Operate (Unit, Op_Dereference, Size);
   end Dereference;

   -----------------
   -- End_Routine --
   -----------------

   procedure End_Routine
     (Unit : in out Tagatha_Unit)
   is
   begin
      for I in 1 .. Unit.Current_Sub.Executable_Segment.Last_Index loop
         Ada.Text_IO.Put_Line
           (Tagatha.Commands.Show
              (Unit.Current_Sub.Executable_Segment.Element (I)));
      end loop;
   end End_Routine;

   -------------------
   -- External_Name --
   -------------------

   function External_Name (Unit : Tagatha_Unit) return String is
      use Ada.Characters.Handling;
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := Unit.Name;
   begin
      for I in 1 .. Length (Result) loop
         if Element (Result, I) = '.' then
            Replace_Slice (Result, I, I, "__");
         end if;
      end loop;
      return To_Lower (To_String (Result));
   end External_Name;

   ----------------------
   -- File_System_Name --
   ----------------------

   function File_System_Name (Unit : Tagatha_Unit) return String is
      use Ada.Characters.Handling;
      use Ada.Strings.Unbounded, Ada.Strings.Maps;
   begin
      return To_Lower
        (To_String (Translate (Unit.Name, To_Mapping (".", "-"))));
   end File_System_Name;

   -----------------
   -- Finish_Unit --
   -----------------

   procedure Finish_Unit (Unit : in out Tagatha_Unit) is
   begin
      pragma Unreferenced (Unit);
   end Finish_Unit;

   -----------------------
   -- Increment_Address --
   -----------------------

   procedure Increment_Address (For_Unit    : in out Tagatha_Unit;
                                By          : in     Positive         := 1)
   is
   begin
      Increment_Address (For_Unit, For_Unit.Current_Segment, By);
   end Increment_Address;

   -----------------------
   -- Increment_Address --
   -----------------------

   procedure Increment_Address (For_Unit    : in out Tagatha_Unit;
                                For_Segment : in     Tagatha_Segment;
                                By          : in     Positive         := 1)
   is
   begin
      For_Unit.Current_Sub.Next_Address (For_Segment) :=
        For_Unit.Current_Sub.Next_Address (For_Segment) + By;
   end Increment_Address;

   ----------
   -- Jump --
   ----------

   procedure Jump (Unit      : in out Tagatha_Unit;
                   Target    : in     Integer;
                   Condition : in     Tagatha_Condition := C_Always)

   is
      Label : Tagatha.Labels.Tagatha_Label;
   begin
      Tagatha.Labels.Reference_Label (Unit.Labels, Label,
                                      Target);
      Append (Unit, Tagatha.Commands.Jump (Label, Condition));
   end Jump;

   -----------
   -- Label --
   -----------

   procedure Label (Unit   : in out Tagatha_Unit;
                    Name   : in     String;
                    Export : in     Boolean := False)
   is
      use Tagatha.Labels;
      Label : Tagatha_Label;
   begin
      Create_Label
        (In_List    => Unit.Labels,
         Label      => Label,
         Linked_To  => Unit.Last_Label (Unit.Current_Segment),
         Segment    => Unit.Current_Segment,
         Location   => Unit.Current_Sub.Next_Address (Unit.Current_Segment),
         Name       => Name,
         Export     => Export);
      Unit.Last_Label (Unit.Current_Segment) := Label;
   end Label;

   -----------
   -- Label --
   -----------

   procedure Label (Unit   : in out Tagatha_Unit;
                    Index  : in     Positive)
   is
      use Tagatha.Labels;
      Label : Tagatha_Label;
   begin
      Create_Label
        (In_List    => Unit.Labels,
         Label      => Label,
         Linked_To  => Unit.Last_Label (Unit.Current_Segment),
         Location   => Unit.Current_Sub.Next_Address (Executable),
         Index      => Index);
      Unit.Last_Label (Unit.Current_Segment) := Label;
   end Label;

   -----------------
   -- Loop_Around --
   -----------------

   procedure Loop_Around (Unit         : in out Tagatha_Unit;
                          Label_Name   : in     String;
                          Loop_Count   : in     Local_Offset;
                          Loop_Index   : in     Local_Offset)
   is
      Label : Tagatha.Labels.Tagatha_Label;
   begin
      Tagatha.Labels.Reference_Label (Unit.Labels, Label,
                                      Label_Name);
      Append (Unit,
              Tagatha.Commands.Loop_Around (Label, Loop_Count, Loop_Index));
   end Loop_Around;

   ----------------
   -- Next_Label --
   ----------------

   procedure Next_Label (Unit   : in out Tagatha_Unit;
                         Index  :    out Positive)
   is
   begin
      Index := Unit.Next_Label;
      Unit.Next_Label :=
        Unit.Next_Label + 1;
   end Next_Label;

   -------------
   -- Operate --
   -------------

   procedure Operate (Unit   : in out Tagatha_Unit;
                      Op     : Tagatha_Operator;
                      Size   : Tagatha_Size       := Default_Integer_Size)
   is
   begin
      Append (Unit, Tagatha.Commands.Operate (Op, False, Size));
   end Operate;

   --------------
   -- Optimise --
   --------------

   procedure Optimise (Unit : in out Tagatha_Unit) is
      Registry : Tagatha.Registry.Tagatha_Registry;
      Start_Label : Tagatha.Labels.Tagatha_Label;
   begin
      Tagatha.Labels.Create_Label
        (In_List    => Unit.Labels,
         Label      => Start_Label,
         Linked_To  => Tagatha.Labels.No_Label,
         Segment    => Executable,
         Location   => 1,
         Name       =>
           Ada.Strings.Unbounded.To_String
             (Unit.Name)
           & "__"
           & Ada.Strings.Unbounded.To_String
              (Unit.Current_Sub.Name),
         Export     => True);

      Registry.Start (Start_Label,
                      Unit.Current_Sub.Frame_Words);
      for I in 1 .. Unit.Current_Sub.Next_Address (Executable) - 1 loop
         declare
            Command : constant Tagatha.Commands.Tagatha_Command :=
              Unit.Current_Sub.Executable_Segment.Element (I);
         begin
            Tagatha.Commands.Register_Command (Registry, Command);
         end;
      end loop;

      Unit.Current_Sub.Transfers :=
        new Tagatha.Transfers.Array_Of_Transfers'(Registry.Get_Transfers);
   end Optimise;

   ------------------
   -- Pop_Argument --
   ------------------

   procedure Pop_Argument
     (Unit       : in out Tagatha_Unit;
      Offset     : in     Argument_Offset;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Pop (Operands.Argument_Operand (Offset), Size));
   end Pop_Argument;

   ---------------
   -- Pop_Label --
   ---------------

   procedure Pop_Label
     (Unit       : in out Tagatha_Unit;
      Label_Name : in     String;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
      Label : Tagatha.Labels.Tagatha_Label;
   begin
      Tagatha.Labels.Reference_Label (Unit.Labels, Label,
                                      Label_Name);
      Append (Unit,
              Commands.Pop (Operands.Label_Operand (Label), Size));
   end Pop_Label;

   ---------------
   -- Pop_Local --
   ---------------

   procedure Pop_Local
     (Unit       : in out Tagatha_Unit;
      Offset     : in     Local_Offset;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Pop (Operands.Local_Operand (Offset), Size));
   end Pop_Local;

   -----------------
   -- Pop_Operand --
   -----------------

   procedure Pop_Operand (Unit      : in out Tagatha_Unit;
                          Op        : in     Tagatha.Operands.Tagatha_Operand;
                          Size      : in     Tagatha_Size)
   is
   begin
      Append (Unit, Commands.Pop (Op, Size));
   end Pop_Operand;

   ----------------
   -- Pop_Result --
   ----------------

   procedure Pop_Result
     (Unit       : in out Tagatha_Unit;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Pop (Operands.Result_Operand, Size));
   end Pop_Result;

   ----------
   -- Push --
   ----------

   procedure Push (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Integer;
                   Size    : in     Tagatha_Size     := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Push (Operands.Constant_Operand (Value), Size));
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Floating_Point)
   is
   begin
      Append (Unit,
        Commands.Push (Operands.Constant_Operand (Value),
          Default_Size));
   end Push;

   -------------------
   -- Push_Argument --
   -------------------

   procedure Push_Argument
     (Unit       : in out Tagatha_Unit;
      Offset     : in     Argument_Offset;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Push (Operands.Argument_Operand (Offset), Size));
   end Push_Argument;

   ---------------------------
   -- Push_Argument_Address --
   ---------------------------

   --  procedure Push_Argument_Address
   --    (Unit       : in out Tagatha_Unit;
   --     Offset     : in     Argument_Offset)
   --  is
   --  begin
   --     Append (Unit,
   --             Commands.Push (Operands.Argument_Operand (Offset), Size));
   --  end Push_Argument_Address;

   ----------------
   -- Push_Label --
   ----------------

   procedure Push_Label
     (Unit       : in out Tagatha_Unit;
      Label_Name : in     String;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
      Label : Tagatha.Labels.Tagatha_Label;
   begin
      Tagatha.Labels.Reference_Label (Unit.Labels, Label,
                                      Label_Name);
      Append (Unit,
              Commands.Push (Operands.Label_Operand (Label), Size));
   end Push_Label;

   ------------------------
   -- Push_Label_Address --
   ------------------------

--     procedure Push_Label_Address
--       (Unit       : in out Tagatha_Unit;
--        Label_Name : in     String)
--     is
--        Label : Tagatha.Labels.Tagatha_Label;
--     begin
--        Tagatha.Labels.Reference_Label (Unit.Unit.Labels, Label,
--                                        Label_Name);
--        Append (Unit, Make_Stack_Operation (S_Push, Tagatha_Address_Size,
--          (O_Label, True, Label)));
--     end Push_Label_Address;

   ----------------
   -- Push_Local --
   ----------------

   procedure Push_Local
     (Unit       : in out Tagatha_Unit;
      Offset     : in     Local_Offset;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Push (Operands.Local_Operand (Offset), Size));
   end Push_Local;

   ------------------
   -- Push_Operand --
   ------------------

   procedure Push_Operand (Unit      : in out Tagatha_Unit;
                           Op        : in     Operands.Tagatha_Operand;
                           Size      : in     Tagatha_Size)
   is
   begin
      Append (Unit, Commands.Push (Op, Size));
   end Push_Operand;

   ------------------------
   -- Push_Local_Address --
   ------------------------

--     procedure Push_Local_Address
--       (Unit       : in out Tagatha_Unit;
--        Offset     : in     Local_Offset)
--     is
--     begin
--        Append (Unit, Make_Stack_Operation (S_Push, Tagatha_Address_Size,
--          (O_Local, True, Offset)));
--     end Push_Local_Address;

   -------------
   -- Segment --
   -------------

   procedure Segment (Unit  : in out Tagatha_Unit;
                      Seg   : in     Tagatha_Segment)
   is
   begin
      Unit.Current_Segment := Seg;
   end Segment;

   -----------
   -- Write --
   -----------

   procedure Write (Unit   : in     Tagatha_Unit;
                    Target : in out Tagatha.Code.Translator'Class)
   is
      use Tagatha.File_Assembly;
      use type Tagatha.Labels.Tagatha_Label;
      File   : File_Assembly_Type;
   begin
      Open (File, Unit.File_System_Name & ".s");
      Target.File_Preamble (File_Assembly_Type'Class (File),
                            Ada.Strings.Unbounded.To_String
                              (Unit.Source_File));
      for Sub of Unit.Subprograms loop

         Target.Start (File_Assembly_Type'Class (File),
                       Subprogram_Name (Unit, Sub), True);

         Target.Begin_Frame (File_Assembly_Type'Class (File),
                             Sub.Argument_Words,
                             Sub.Frame_Words);
         for I in Sub.Transfers.all'Range loop
            if I = Sub.Transfers'Last
              and then Unit.Last_Label (Executable) /= Tagatha.Labels.No_Label
            then
               Target.Label (File_Assembly_Type'Class (File),
                             Unit.Last_Label (Executable));
            end if;
            Target.Encode (File_Assembly_Type'Class (File),
                           Sub.Transfers (I));
         end loop;

         Target.End_Frame (File_Assembly_Type'Class (File),
                           Sub.Argument_Words,
                           Sub.Frame_Words);
         Target.Finish (File_Assembly_Type'Class (File));
      end loop;

      for Sub of Unit.Subprograms loop

         for Datum of Sub.Read_Only_Segment loop

            if Datum.Label /= Tagatha.Labels.No_Label then
               Target.Label (File_Assembly_Type'Class (File),
                             Datum.Label);
            end if;

            case Datum.Data_Type is
               when Integer_Data =>
                  Target.Data (File_Assembly_Type'Class (File),
                               Tagatha.Constants.Integer_Constant
                                 (Datum.Integer_Value));
               when Floating_Point_Data =>
                  Target.Data (File_Assembly_Type'Class (File),
                               Tagatha.Constants.Floating_Point_Constant
                                 (Datum.Floating_Point_Value));
               when Label_Data =>
                  Target.Data (File_Assembly_Type'Class (File),
                               Tagatha.Constants.Label_Constant
                                 (Datum.Label_Value));
               when String_Data =>
                  null;
            end case;
         end loop;
      end loop;

      Target.File_Postamble (File_Assembly_Type'Class (File));
      Close (File);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write (Unit        : in     Tagatha_Unit;
                    Target_Name : in String)
   is
      Target : Tagatha.Code.Translator'Class :=
                 Tagatha.Code.Get_Translator (Target_Name);
   begin
      Write (Unit, Target);
   end Write;

end Tagatha.Units;
