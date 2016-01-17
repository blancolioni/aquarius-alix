with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;

package body Tagatha.Transfers.Optimiser is

   package Known_Value_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
        Element_Type    => Tagatha.Transfers.Transfer_Operand,
        Hash            => Ada.Strings.Unbounded.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Unbounded.Equal_Case_Insensitive);

   --------------
   -- Optimise --
   --------------

   procedure Optimise
     (Transfers : in out Tagatha.Transfers.Transfer_Vectors.Vector)
   is
      From_Index : Positive := 1;
      Known_Values : Known_Value_Maps.Map;

      Result : Tagatha.Transfers.Transfer_Vectors.Vector;
      Label        : Tagatha.Labels.Tagatha_Label := Tagatha.Labels.No_Label;

      procedure Clear_Known_Registers
        (Changed_Registers : String)
      is null;

   begin
      while From_Index <= Transfers.Last_Index loop
         declare
            From : Transfer := Transfers (From_Index);
            Copy : Boolean  := True;
         begin
            if Tagatha.Labels.Has_Label (From.Label) then
               Known_Values.Clear;
            end if;

            if From_Index < Transfers.Last_Index - 1
              and then Transfers (From_Index).Op = Op_Not
              and then Transfers (From_Index + 1).Op = Op_Test
              and then Transfers (From_Index + 2).Trans = T_Control
              and then Transfers (From_Index + 2).Condition /= C_Always
            then
               From := Transfers (From_Index + 2);
               From.Condition := Negate (From.Condition);
               From_Index := From_Index + 2;
            elsif From.Trans = T_Data
              and then From.Op = Op_Nop
              and then Same_Operand (From.Src_1, From.Dst)
            then
               Copy := False;
            elsif From.Trans = T_Data
              and then From.Op = Op_Nop
              and then From.Src_1.Op /= T_Stack
              and then (From.Src_1.Op /= T_External
                        or else From.Src_1.External_Imm)
              and then From.Dst.Op = T_External
              and then Known_Values.Contains (From.Dst.External_Name)
              and then Known_Values (From.Dst.External_Name) = From.Src_1
            then
               Copy := False;
            end if;

            if Copy then
               if Tagatha.Labels.Has_Label (Label) then
                  Tagatha.Labels.Link_To (Label, From.Label);
                  From.Label := Label;
                  Label := Tagatha.Labels.No_Label;
               end if;
               Result.Append (From);
               if From.Trans = T_Data
                 and then From.Op = Op_Nop
                 and then From.Src_1.Op /= T_Stack
                 and then From.Dst.Op = T_External
               then
                  if Known_Values.Contains (From.Dst.External_Name) then
                     Known_Values (From.Dst.External_Name) := From.Src_1;
                  else
                     Known_Values.Insert (From.Dst.External_Name, From.Src_1);
                  end if;
               elsif From.Trans = T_Native then
                  Clear_Known_Registers
                    (Ada.Strings.Unbounded.To_String (From.Changed_Registers));
               end if;
            elsif Tagatha.Labels.Has_Label (From.Label) then
               Tagatha.Labels.Link_To (From.Label, Label);
               Label := From.Label;
               Known_Values.Clear;
            end if;

            From_Index := From_Index + 1;

         end;
      end loop;

      Transfers := Result;

   end Optimise;

end Tagatha.Transfers.Optimiser;
