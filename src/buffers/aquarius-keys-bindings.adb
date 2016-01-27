with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;

package body Aquarius.Keys.Bindings is

   type Key_Binding_Record is
      record
         Sequence : Sequences.Key_Sequence;
         Binding  : Bound_Object;
      end record;

   package Key_Binding_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Key_Binding_Record);

   type Top_Level_Entry is
      record
         Have_Binding : Boolean;
         Binding      : Bound_Object;
         Rest         : Key_Binding_Lists.List;
      end record;

   package Key_Binding_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Aquarius_Key,
        Element_Type => Top_Level_Entry);

   type Binding_Table_Record is
      record
         Map : Key_Binding_Maps.Map;
      end record;

   -----------------
   -- Add_Binding --
   -----------------

   procedure Add_Binding
     (Table : Binding_Table;
      Sequence : Aquarius.Keys.Sequences.Key_Sequence;
      Binding  : Bound_Object)
   is
      use Key_Binding_Maps;
      Keys : constant Sequences.Array_Of_Keys :=
               Sequences.Keys (Sequence);
      Position : constant Cursor :=
                   Table.Table.Map.Find (Keys (Keys'First));
   begin
      if Has_Element (Position) then
         declare
            Elem : Top_Level_Entry := Element (Position);
         begin
            if Keys'Length = 1 then
               Elem :=
                 (Have_Binding => True, Binding => Binding,
                  Rest         => Key_Binding_Lists.Empty_List);
            else
               if Element (Position).Have_Binding then
                  Elem.Rest.Clear;
                  Elem.Rest.Append ((Sequence, Binding));
                  Elem.Have_Binding := False;
               else
                  declare
                     Replaced : Boolean := False;
                  begin
                     for Check_Position in Elem.Rest.Iterate loop
                        declare
                           use Key_Binding_Lists;
                           use type Sequences.Array_Of_Keys;
                           Seq : constant Sequences.Array_Of_Keys :=
                                   Sequences.Keys
                                     (Element (Check_Position).Sequence);
                        begin
                           if Seq = Keys then
                              Elem.Rest.Replace_Element
                                (Check_Position, (Sequence, Binding));
                              Replaced := True;
                              exit;
                           end if;
                        end;
                     end loop;
                     if not Replaced then
                        Elem.Rest.Append ((Sequence, Binding));
                     end if;
                  end;
               end if;
            end if;

            Table.Table.Map.Replace_Element (Position, Elem);
         end;
      else
         declare
            Elem : Top_Level_Entry;
         begin
            Elem.Have_Binding := Keys'Length = 1;
            if Elem.Have_Binding then
               Elem.Binding := Binding;
            else
               Elem.Rest.Append ((Sequence, Binding));
            end if;

            Table.Table.Map.Insert
              (Keys (1), Elem);
         end;
      end if;
   end Add_Binding;

   ------------
   -- Create --
   ------------

   procedure Create (Table : in out Binding_Table) is
   begin
      Table.Table := new Binding_Table_Record;
   end Create;

   -----------------
   -- Get_Binding --
   -----------------

   procedure Get_Binding
     (Table      : Binding_Table;
      Sequence   : Aquarius.Keys.Sequences.Key_Sequence;
      Incomplete : out Boolean;
      Match      : out Boolean;
      Binding    : out Bound_Object)
   is
      use Key_Binding_Maps;
      Keys : constant Sequences.Array_Of_Keys :=
               Sequences.Keys (Sequence);
      Position : constant Cursor :=
                   Table.Table.Map.Find (Keys (Keys'First));
   begin
      if not Has_Element (Position) then
         Incomplete := False;
         Match := False;
      elsif Element (Position).Have_Binding then
         Incomplete := False;
         Match := True;
         Binding := Element (Position).Binding;
      else
         Match := False;
         Incomplete := False;
         for Check_Binding of Element (Position).Rest loop
            declare
               use type Sequences.Array_Of_Keys;
               Seq : constant Sequences.Array_Of_Keys :=
                       Sequences.Keys (Check_Binding.Sequence);
            begin
               if Seq = Keys then
                  Match := True;
                  Binding := Check_Binding.Binding;
                  exit;
               elsif Seq'Length < Keys'Length
                 and then Seq = Keys (1 .. Seq'Length)
               then
                  Incomplete := True;
                  exit;
               end if;
            end;
         end loop;
      end if;
   end Get_Binding;

end Aquarius.Keys.Bindings;
