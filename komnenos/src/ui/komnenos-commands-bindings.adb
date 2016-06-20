with Aquarius.Keys.Sequences;

package body Komnenos.Commands.Bindings is

   ----------------------
   -- Default_Bindings --
   ----------------------

   procedure Default_Bindings
     (Table : in out Binding_Table'Class)
   is
      use Aquarius.Keys;

      procedure Bind
        (Key : Aquarius.Keys.Aquarius_Key;
         Name : String);

      ----------
      -- Bind --
      ----------

      procedure Bind
        (Key : Aquarius.Keys.Aquarius_Key;
         Name : String)
      is
         Sequence : Aquarius.Keys.Sequences.Key_Sequence;
      begin
         Aquarius.Keys.Sequences.Add_Key (Sequence, Key);
         Table.Add_Binding
           (Sequence,
            Standard_Table.Get_Reference (Name));
      end Bind;

   begin
      Table.Create;
      Bind (Aquarius.Keys.Up_Arrow, "previous-line");
      Bind (Aquarius.Keys.Down_Arrow, "next-line");
      Bind (Aquarius.Keys.Left_Arrow, "backward-character");
      Bind (Aquarius.Keys.Right_Arrow, "forward-character");

      Bind (Aquarius.Keys.Line_Feed, "new-line");
      Bind (Aquarius.Keys.Carriage_Return, "new-line");

      for Ch in Character range ' ' .. '~' loop
         Bind (Aquarius.Keys.Character_Key (Ch),
               "insert-character" & Integer'Image (-Character'Pos (Ch)));
      end loop;

   end Default_Bindings;

end Komnenos.Commands.Bindings;
