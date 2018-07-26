with Ada.Text_IO;
with Aqua.IO;

package body Aquarius.Programs.Aqua_Driver is

   Trace_Properties : constant Boolean := False;

   Local_Tree_Driver : aliased Aquarius_Tree_Driver_Record (255);

   type Driver_Command is
     (No_Command,
      Get_Name,
      Get_Text,
      Get_Standard_Text,
      Concatenated_Image,
      Error,
      Get_Property,
      Has_Property,
      Set_Property,
      Full_File_Name,
      File_Name,
      Start_Line,
      End_Line,
      Start_Column,
      End_Column,
      Start_Position,
      End_Position);

   function Is_Command (Value : Natural) return Boolean
   is (Value <= Driver_Command'Pos (Driver_Command'Last));

   --------------------------
   -- Aquarius_Tree_Driver --
   --------------------------

   function Aquarius_Tree_Driver
      return Aqua.Drivers.Aqua_Driver
   is
   begin
      return Local_Tree_Driver'Access;
   end Aquarius_Tree_Driver;

   -----------------
   -- Read_String --
   -----------------

   function Read_String
     (Driver : Aquarius_Tree_Driver_Record'Class)
      return String
   is
      use Aqua.Drivers;
      A : Driver_Register_Range := 12;
      L : constant Natural := Natural (Driver.Get_Word (A));
      S : String (1 .. L);
   begin
      for Ch of S loop
         A := A + 4;
         Ch := Character'Val (Driver.Get_Word (A));
      end loop;
      return S;
   end Read_String;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Driver : in out Aquarius_Tree_Driver_Record)
   is
      Current_Sequence : constant Natural :=
                           Natural (Driver.Get_Word (0));
      Command          : constant Natural :=
                           Natural (Driver.Get_Word (4));
   begin

      Driver.Set_Word (4, 0);

      if Current_Sequence = 0 then
         return;
      end if;

      if Driver.Current = null
        or else Driver.Current.Sequence /= Current_Sequence
      then
         Driver.Current := Get_Tree_From_Sequence (Current_Sequence);
      end if;

      if not Is_Command (Command) then
         return;
      end if;

      case Driver_Command'Val (Command) is
         when No_Command =>
            null;
         when Get_Name =>
            Driver.Write_String (Driver.Current.Name);
         when Get_Text =>
            Driver.Write_String (Driver.Current.Text);
         when Get_Standard_Text =>
            Driver.Write_String (Driver.Current.Standard_Text);
         when Concatenated_Image =>
            Driver.Write_String (Driver.Current.Concatenate_Children);
         when File_Name =>
            Driver.Write_String
              (Aquarius.Names.To_String (Driver.Current.Source_File_Name));
         when Full_File_Name =>
            Driver.Write_String
              (Aquarius.Names.To_String (Driver.Current.Source_File_Name));
         when Start_Line =>
            Driver.Set_Word (8, Aqua.Word (Driver.Current.Source_Line));
         when End_Line =>
            Driver.Set_Word (8, Aqua.Word (Driver.Current.End_Line));
         when Start_Column =>
            Driver.Set_Word (8, Aqua.Word (Driver.Current.Source_Column));
         when End_Column =>
            Driver.Set_Word (8, Aqua.Word (Driver.Current.End_Column));
         when Start_Position =>
            Driver.Set_Word (8, Aqua.Word (Driver.Current.Start_Position));
         when End_Position =>
            Driver.Set_Word (8, Aqua.Word (Driver.Current.End_Position));
         when Error =>
            Driver.Current.Attach_Message
              (Aquarius.Messages.New_Message
                 (Level    => Aquarius.Messages.Error,
                  Location => Driver.Current,
                  Text     => Driver.Read_String));
         when Get_Property =>
            declare
               Name  : constant String := Driver.Read_String;
               Props : Aqua_Address_Maps.Map renames
                         Driver.Current.Aqua_Props;
               Value : constant Aqua.Address :=
                         Props.Element (Name);
            begin
               if Trace_Properties then
                  Ada.Text_IO.Put_Line
                    (Driver.Current.Show_Location
                     & ": " & Aqua.IO.Hex_Image (Driver.Get_Word (0))
                     & ": get_property "
                     & Name & " = " & Aqua.IO.Hex_Image (Value));
               end if;

               Driver.Set_Word (8, Value);
            end;
         when Has_Property =>
            declare
               Name  : constant String := Driver.Read_String;
               Props : Aqua_Address_Maps.Map renames
                         Driver.Current.Aqua_Props;
               Value : constant Boolean :=
                         Props.Contains (Name);
            begin
               if Trace_Properties then
                  Ada.Text_IO.Put_Line
                    (Driver.Current.Show_Location
                     & ": " & Aqua.IO.Hex_Image (Driver.Get_Word (0))
                     & ": has_property "
                     & Name & " = " & Boolean'Image (Value));
               end if;

               Driver.Set_Word
                 (8, Boolean'Pos (Value));
            end;
         when Set_Property =>
            declare
               Name  : constant String := Driver.Read_String;
               Props : Aqua_Address_Maps.Map renames
                         Driver.Current.Aqua_Props;
               Value : constant Aqua.Address := Driver.Get_Word (8);
            begin
               if Trace_Properties then
                  Ada.Text_IO.Put_Line
                    (Driver.Current.Show_Location
                     & ": " & Aqua.IO.Hex_Image (Driver.Get_Word (0))
                     & ": set_property "
                     & Name & " := " & Aqua.IO.Hex_Image (Value));
               end if;

               if Props.Contains (Name) then
                  Props.Replace (Name, Value);
               else
                  Props.Insert (Name, Value);
               end if;
            end;
      end case;
   end Update;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String
     (Driver : in out Aquarius_Tree_Driver_Record'Class;
      S      : String)
   is
      use Aqua.Drivers;
      A : Driver_Register_Range := 12;
   begin
      Driver.Set_Word (A, Aqua.Word (S'Length));
      for Ch of S loop
         A := A + 4;
         Driver.Set_Word (A, Character'Pos (Ch));
      end loop;
   end Write_String;

end Aquarius.Programs.Aqua_Driver;
