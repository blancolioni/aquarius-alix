with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;
with Ada.Text_IO;

with Ada.Containers.Indefinite_Hashed_Maps;

package body Aquarius.Transformers is

   package Transformer_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Root_Transformer_Type'Class,
        Hash            => Hash_Case_Insensitive,
        Equivalent_Keys => "=");

   Transformer_Map : Transformer_Maps.Map;

   -------------------------
   -- Execute_Transformer --
   -------------------------

   procedure Execute_Transformer
     (Transformer      : Root_Transformer_Type'Class;
      Source_Path      : String;
      Destination_Path : String)
   is
      use Ada.Strings, Ada.Strings.Fixed;
      use Ada.Text_IO;
      Source, Destination : File_Type;
   begin
      Open (Source, In_File, Source_Path);
      Create (Destination, Out_File, Destination_Path);
      Set_Output (Destination);

      while not End_Of_File (Source) loop
         declare
            Full_Line : constant String := Get_Line (Source);
            Line : constant String := Trim (Full_Line, Both);
         begin
            if Line'Length = 0 then
               Transformer.On_Text_Line ("");
            elsif Line (Line'First) = '@' then
               if Line'Length = 1 then
                  Transformer.On_Script_Line ("");
               elsif Line (Line'First + 1) = '!' then
                  null;
               else
                  Transformer.On_Script_Line
                    (Line (Line'First + 1 .. Line'Last));
               end if;
            else
               declare
                  Start : Positive := Full_Line'First;
               begin
                  while Start <= Full_Line'Last loop
                     declare
                        Next : constant Natural :=
                                 Index (Full_Line, "[", Start);
                        Finish : constant Natural :=
                                   (if Next = 0 then 0
                                    else Index (Full_Line, "]", Next + 1));
                     begin
                        if Next = 0 or else Finish = 0 then
                           Transformer.On_Text
                             (Full_Line (Start .. Full_Line'Last));
                           Start := Full_Line'Last + 1;
                        else
                           if Next > Start then
                              Transformer.On_Text
                                (Text => Full_Line (Start .. Next - 1));
                           end if;
                           Transformer.On_Script_Inline
                             (Full_Line (Next + 1 .. Finish - 1));
                           Start := Finish + 1;
                        end if;
                     end;
                  end loop;
                  Transformer.On_Text_Line ("");
               end;
            end if;
         end;
      end loop;

      Set_Output (Standard_Output);
      Close (Destination);
      Close (Source);

   exception
      when others =>
         Set_Output (Standard_Output);
         raise;
   end Execute_Transformer;

   ---------------------
   -- Get_Transformer --
   ---------------------

   function Get_Transformer
     (Name : String)
      return Root_Transformer_Type'Class
   is
      Key : constant Unbounded_String :=
              To_Unbounded_String (Name);
   begin
      if Transformer_Map.Contains (Key) then
         return Transformer_Map.Element (Key);
      else
         raise Constraint_Error with
         Name & ": no such transformer";
      end if;
   end Get_Transformer;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name        : String;
      Transformer : Root_Transformer_Type'Class)
   is
   begin
      Transformer_Map.Insert (To_Unbounded_String (Name), Transformer);
   end Register;

end Aquarius.Transformers;
