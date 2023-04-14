with Ada.Text_IO;

with As.Expressions;
with As.Files;
with As.Images;
with As.Instructions;
with As.Parser;
with As.Paths;

package body As.Assembler is

   procedure Error
     (This    : in out Instance'Class;
      Context : As.Files.File_Context;
      Message : String);

   --------------
   -- Assemble --
   --------------

   procedure Assemble (This : in out Instance'Class) is

      PC  : Word_32 := 0;

      procedure Pass_One (Line : As.Source.Source_Line);
      procedure Pass_Two (Line : As.Source.Source_Line);

      function Arguments
        (Line : As.Source.Source_Line)
         return As.Instructions.Instruction_Arguments;

      ---------------
      -- Arguments --
      ---------------

      function Arguments
        (Line : As.Source.Source_Line)
         return As.Instructions.Instruction_Arguments
      is
         use As.Source;
      begin
         return Args : As.Instructions.Instruction_Arguments
           (1 .. Get_Argument_Count (Line))
         do
            for I in 1 .. Get_Argument_Count (Line) loop
               Args (I) :=
                 As.Instructions.Expression_Reference
                   (Get_Argument (Line, I));
            end loop;
         end return;
      end Arguments;

      --------------
      -- Pass_One --
      --------------

      procedure Pass_One (Line : As.Source.Source_Line) is
         use As.Source;
      begin

         This.Env.Set_Location (PC);

         if Has_Command (Line)
           and then Get_Command (Line) = "equ"
         then
            pragma Assert (Get_Argument_Count (Line) = 1);
            if Get_Label (Line) = "@" then
               declare
                  pragma Assert
                    (Get_Argument (Line, 1).Has_Word_Value (This.Env));
                  Loc : constant Word_32 :=
                          Get_Argument (Line, 1).Get_Word_Value (This.Env);
               begin
                  PC := Loc;
               end;
            else
               This.Env.Insert (Get_Label (Line), Get_Argument (Line, 1));
            end if;
         elsif Has_Command (Line)
           and then Get_Command (Line) = "global"
         then
            pragma Assert (Get_Argument_Count (Line) = 1);
            pragma Assert (Get_Argument (Line, 1).Has_Word_Value (This.Env));
            This.Env.Insert_Global
              (Context (Line), Get_Label (Line),
               Get_Argument (Line, 1).Get_Word_Value (This.Env));
         else

            declare
               Label : constant String := Get_Label (Line);
            begin
               if Label /= "" then
                  if Has_Local_Label (Line) then
                     This.Env.Insert_Local_Label
                       (Positive'Value (Label));
                  else
                     if This.Env.Contains (Label) then
                        if This.Env.Has_Value (Label) then
                           This.Error (Context (Line),
                                       "redefined label: " & Label);
                        else
                           This.Env.Update (Label, PC);
                        end if;
                     else
                        This.Env.Insert (Label, PC);
                     end if;
                  end if;
               end if;
            end;

            if Has_Command (Line) then
               declare
                  Command : constant String := Get_Command (Line);
               begin
                  if not This.Env.Contains (Command) then
                     This.Error (Context (Line), "undefined: " & Command);
                  elsif not This.Env.Has_Value (Command) then
                     This.Error (Context (Line), "not a command: " & Command);
                  else
                     declare
                        Value : constant As.Expressions.Reference :=
                                  This.Env.Get_Value (Command);
                     begin
                        if Value.Has_Instruction_Value (This.Env) then
                           declare
                              Instr : constant As.Instructions.Reference :=
                                        Value.Get_Instruction_Value (This.Env);
                              Align : constant Word_32 := Instr.Alignment;
                           begin
                              if PC mod Align /= 0 then
                                 PC := PC + Align - PC mod Align;
                              end if;
                              Instr.Skip (PC, This.Env, Arguments (Line));
                           end;
                        else
                           This.Error (Context (Line),
                                       "invalid command: " & Command);
                        end if;
                     end;
                  end if;
               end;
            end if;
         end if;
      end Pass_One;

      --------------
      -- Pass_Two --
      --------------

      procedure Pass_Two (Line : As.Source.Source_Line) is
         use As.Source;
      begin

         This.Env.Set_Location (This.Obj.Location);
         This.Obj.Set_Context (As.Source.Context (Line));

         if Has_Command (Line)
           and then Get_Command (Line) = "equ"
           and then Get_Label (Line) = "@"
         then
            declare
               Loc : constant Word_32 :=
                       Get_Argument (Line, 1).Get_Word_Value (This.Env);
            begin
               This.Obj.Set_Location (Loc);
            end;
         elsif Has_Command (Line)
           and then Get_Command (Line) /= "equ"
           and then Get_Command (Line) /= "global"
         then
            declare
               Command : constant String := Get_Command (Line);
               Value   : constant As.Expressions.Reference :=
                           This.Env.Get_Value (Command);
               Args    : constant As.Instructions.Instruction_Arguments :=
                           Arguments (Line);
            begin
               if Value.Has_Instruction_Value (This.Env) then
                  declare
                     Instr : constant As.Instructions.Reference :=
                               Value.Get_Instruction_Value (This.Env);
                     Align : constant Word_32 := Instr.Alignment;
                  begin
                     while This.Obj.Location mod Align /= 0 loop
                        This.Obj.Append (Word_8'(0));
                     end loop;
                     Value.Get_Instruction_Value (This.Env)
                       .Assemble (This.Env, Args, This.Obj);
                  exception
                     when others =>
                        raise Constraint_Error with
                        As.Files.To_String (Context (Line))
                          & ": error assembling "
                          & Value.Get_Instruction_Value (This.Env).To_String;
                  end;

               end if;
            end;
         end if;
      end Pass_Two;

   begin

      for Source of This.Sources loop
         Source.Iterate (Pass_One'Access);
      end loop;

      This.Obj := As.Objects.Create;
      PC := 0;

      for Source of This.Sources loop
         Source.Iterate (Pass_Two'Access);
      end loop;

      This.Obj.Globals (This.Env.Last_Global);
      for G in This.Env.Last_Global .. 254 loop
         This.Obj.Global_Value (This.Env.Get_Global (G));
      end loop;
      This.Obj.Global_Value (4096);
   end Assemble;

   -----------
   -- Error --
   -----------

   procedure Error
     (This    : in out Instance'Class;
      Context : As.Files.File_Context;
      Message : String)
   is
      pragma Unreferenced (This);
   begin
      Ada.Text_IO.Put_Line
        (As.Files.To_String (Context)
         & ": " & Message);
   end Error;

   ----------
   -- List --
   ----------

   procedure List
     (This : in out Instance'Class;
      Path : String)
   is

      procedure List_Data (Context : As.Files.File_Context);
      procedure List_Line (Line : As.Source.Source_Line);

      ---------------
      -- List_Data --
      ---------------

      procedure List_Data (Context : As.Files.File_Context) is
         G : Register_Index := 0;
         Value : Word_32;
      begin
         This.Env.Get_Global (Context, G, Value);
         if G /= 0 then
            Ada.Text_IO.Put (As.Images.Hex_Image (Value));
            Ada.Text_IO.Put (Register_Index'Image (G));
         elsif This.Obj.Has_Context (Context) then
            declare
               Address : constant Word_32 :=
                           This.Obj.Get_Address (Context);
               Data    : constant As.Objects.Context_Data :=
                           This.Obj.Get_Data (Context);
               Count   : Natural := 0;
            begin
               Ada.Text_IO.Put (As.Images.Hex_Image (Address));
               for D of Data loop
                  exit when Count >= 4;
                  Ada.Text_IO.Put (" " & As.Images.Hex_Image (D));
                  Count := Count + 1;
               end loop;
            end;
         end if;
      end List_Data;

      ---------------
      -- List_Line --
      ---------------

      procedure List_Line (Line : As.Source.Source_Line) is

         use As.Source;

         First_Argument : Boolean := True;

         procedure List_Argument (Argument : As.Expressions.Reference);

         -------------------
         -- List_Argument --
         -------------------

         procedure List_Argument (Argument : As.Expressions.Reference) is
         begin
            if First_Argument then
               First_Argument := False;
            else
               Ada.Text_IO.Put (",");
            end if;
            Ada.Text_IO.Put (" " & Argument.To_String);
         end List_Argument;

      begin

         if Has_Command (Line) then
            List_Data (As.Source.Context (Line));
         end if;

         if As.Source.Get_Label (Line) /= "" then
            Ada.Text_IO.Set_Col (24);
            Ada.Text_IO.Put (As.Source.Get_Label (Line));
         end if;

         if As.Source.Has_Command (Line) then
            Ada.Text_IO.Set_Col (40);
            Ada.Text_IO.Put (As.Source.Get_Command (Line));
         end if;

         for I in 1 .. As.Source.Get_Argument_Count (Line) loop
            List_Argument (As.Source.Get_Argument (Line, I));
         end loop;

         Ada.Text_IO.New_Line;
      end List_Line;

      List_File : Ada.Text_IO.File_Type;

   begin

      Ada.Text_IO.Create (List_File, Ada.Text_IO.Out_File, Path);
      Ada.Text_IO.Set_Output (List_File);

      for Source of This.Sources loop
         Source.Iterate (List_Line'Access);
      end loop;

      declare
         procedure Show_Label (Name : String);

         ----------------
         -- Show_Label --
         ----------------

         procedure Show_Label (Name : String) is
         begin
            Ada.Text_IO.Put (Name);
            if This.Env.Has_Value (Name) then
               Ada.Text_IO.Set_Col (24);
               Ada.Text_IO.Put (This.Env.Get_Value (Name).To_String);
            end if;
            Ada.Text_IO.New_Line;
         end Show_Label;

      begin
         This.Env.Iterate (Show_Label'Access);
      end;

      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
      Ada.Text_IO.Close (List_File);

   end List;

   ----------
   -- Load --
   ----------

   procedure Load (This : in out Instance'Class; Path : String) is
      Source : constant As.Source.Reference :=
                 As.Parser.Load (Path);
   begin
      This.Sources.Append (Source);
   end Load;

   -------------------
   -- New_Assembler --
   -------------------

   function New_Assembler return Reference is
   begin
      return This : constant Reference :=
        new Instance'
          (Env => As.Environment.Create,
           others => <>)
      do
         This.Load (As.Paths.Config_File ("const.s"));
         --  This.Load (As.Paths.Config_File ("artl.s"));
      end return;
   end New_Assembler;

   -----------
   -- Write --
   -----------

   procedure Write (This : Instance'Class; Path : String) is
   begin
      This.Obj.Write (Path);
   end Write;

end As.Assembler;
