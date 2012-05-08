with Tagatha.Fragments;

with Aquarius.Entries.Objects;

package body Ada_Plugin.Names.Gen is

   -----------------------
   -- Direct_Name_After --
   -----------------------

   procedure Direct_Name_After (Name : Aquarius.Programs.Program_Tree) is
      E    : Aquarius.Entries.Table_Entry;
   begin
      if Name.Has_Entry then
         E := Name.Get_Entry;
         if Aquarius.Entries.Objects.Is_Object_Entry (E) then
            declare
               Offset : constant Integer :=
                 Aquarius.Entries.Objects.Frame_Offset (E);
            begin
               if Offset > 0 then
                  --  argument
                  Name.Set_Fragment
                    (Tagatha.Fragments.Reference_Argument
                       (Tagatha.Argument_Offset (Offset)));
               else
                  --  local variable
                  Name.Set_Fragment
                    (Tagatha.Fragments.Reference_Local
                       (Tagatha.Local_Offset (-1 * Offset)));
               end if;
            end;
         end if;
      end if;
   end Direct_Name_After;

   ----------------------------
   -- Object_Reference_After --
   ----------------------------

   procedure Object_Reference_After (Ref : Aquarius.Programs.Program_Tree) is
      use Aquarius.Programs;
      Qualifiers : constant Array_Of_Program_Trees :=
        Ref.Direct_Children ("name_qualifier");
      Direct     : constant Program_Tree :=
        Ref.Program_Child ("direct_name");
      Last       : Program_Tree;
   begin
      if Qualifiers'Length > 0 then
         Last := Qualifiers (Qualifiers'Last);
      else
         Last := Direct;
      end if;

      Ref.Set_Fragment (Last.Get_Fragment);
   end Object_Reference_After;

end Ada_Plugin.Names.Gen;
