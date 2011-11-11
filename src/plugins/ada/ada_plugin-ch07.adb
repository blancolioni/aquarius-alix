with Aquarius.Entries.Packages;

package body Ada_Plugin.Ch07 is

   use Aquarius.Programs;

   -------------------------------------
   -- Package_Spec_After_Package_Name --
   -------------------------------------

   procedure Package_Spec_After_Defining_Package_Name
     (Package_Spec : in Aquarius.Programs.Program_Tree;
      Package_Name : in Aquarius.Programs.Program_Tree)
   is
   begin
      declare
         use Aquarius.Entries;

         Reference     : constant Program_Tree :=
           Package_Name.Program_Child ("defining_qualified_reference");
         Defining_Name : constant Aquarius.Programs.Program_Tree :=
           Program_Tree
           (Reference.Property (Plugin.Last_Identifier_Property));
         Package_Entry : Aquarius.Entries.Table_Entry;
         Parent_Entry  : Aquarius.Entries.Table_Entry;
         Compilation_Unit : constant Program_Tree :=
           Program_Tree (Package_Spec.Property
                           (Plugin.Compilation_Unit_Property));
      begin
         if Reference.Has_Entry then
            Parent_Entry := Reference.Get_Entry;
         end if;

         Package_Spec.Create_Symbol_Table;

         Package_Entry :=
           Aquarius.Entries.Packages.New_Package_Entry
           (Defining_Name.Standard_Text,
            Parent_Entry,
            Package_Name,
            Package_Spec.Symbol_Table);
         Package_Spec.Set_Entry (Package_Entry);

         if not Compilation_Unit.Has_Entry then
            Compilation_Unit.Set_Entry (Package_Entry);
         end if;

         Compilation_Unit.Symbol_Table.Insert (Package_Entry);
      end;
   end Package_Spec_After_Defining_Package_Name;

end Ada_Plugin.Ch07;
