with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;

with Ada.Strings.Unbounded.Equal_Case_Insensitive;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;

package body Komnenos.Entities.Visuals is

   package List_Of_Visuals is
     new Ada.Containers.Doubly_Linked_Lists (Entity_Visual_Access);

   package Entity_Visual_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
        Element_Type    => List_Of_Visuals.List,
        Hash            => Ada.Strings.Unbounded.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Unbounded.Equal_Case_Insensitive,
        "="             => List_Of_Visuals."=");

   Bound_Entity_Map : Entity_Visual_Maps.Map;

   -----------------
   -- Bind_Visual --
   -----------------

   procedure Bind_Visual
     (Visual : not null access Entity_Visual'Class;
      Entity : not null access Root_Entity_Reference'Class)
   is
      List : List_Of_Visuals.List;
   begin
      if not Bound_Entity_Map.Contains (Entity.Key) then
         List.Append (Entity_Visual_Access (Visual));
         Bound_Entity_Map.Insert (Entity.Key, List);
      else
         List := Bound_Entity_Map.Element (Entity.Key);
         List.Append (Entity_Visual_Access (Visual));
         Bound_Entity_Map.Replace (Entity.Key, List);
      end if;
   end Bind_Visual;

   ----------------------
   -- Delete_At_Cursor --
   ----------------------

   procedure Delete_At_Cursor
     (Entity : in out Root_Entity_Reference'Class;
      Cursor : Cursor_Type;
      Offset : Aquarius.Layout.Position_Offset)
   is
      pragma Unreferenced (Cursor);
   begin
      if Bound_Entity_Map.Contains (Entity.Key) then
         declare
            List : constant List_Of_Visuals.List :=
                     Bound_Entity_Map.Element (Entity.Key);
         begin
            for Visual of List loop
               Visual.Delete_From_Cursor (Offset);
            end loop;
         end;
      end if;
   end Delete_At_Cursor;

   ----------------------
   -- Insert_At_Cursor --
   ----------------------

   procedure Insert_At_Cursor
     (Entity : in out Root_Entity_Reference'Class;
      Cursor : Cursor_Type;
      Text   : String)
   is
      pragma Unreferenced (Cursor);
   begin
      if Bound_Entity_Map.Contains (Entity.Key) then
         declare
            List : constant List_Of_Visuals.List :=
                     Bound_Entity_Map.Element (Entity.Key);
         begin
            for Visual of List loop
               Visual.Insert_At_Cursor (Text);
            end loop;
         end;
      end if;
   end Insert_At_Cursor;

   ------------------------
   -- Invalidate_Visuals --
   ------------------------

   procedure Invalidate_Visuals
     (Entity : in out Root_Entity_Reference'Class)
   is
   begin
      if Bound_Entity_Map.Contains (Entity.Key) then
         declare
            List : constant List_Of_Visuals.List :=
                     Bound_Entity_Map.Element (Entity.Key);
         begin
            for Visual of List loop
               Visual.Invalidate;
            end loop;
         end;
      end if;
   end Invalidate_Visuals;

   -------------------
   -- Unbind_Visual --
   -------------------

   procedure Unbind_Visual
     (Visual : not null access Entity_Visual'Class)
   is
      Entity : constant Entity_Reference := Visual.Get_Content;
   begin
      if not Bound_Entity_Map.Contains (Entity.Key) then
         raise Constraint_Error with
           "entity " & Ada.Strings.Unbounded.To_String (Entity.Key)
           & " has no visuals";
      else
         declare
            List : List_Of_Visuals.List :=
                     Bound_Entity_Map.Element (Entity.Key);
            Position : List_Of_Visuals.Cursor :=
                         List.Find (Entity_Visual_Access (Visual));
         begin
            if not List_Of_Visuals.Has_Element (Position) then
               raise Constraint_Error with
                 "attempted to remove visual from " & Entity.Name
                 & " which was not found";
            else
               List.Delete (Position);
               Bound_Entity_Map.Replace (Entity.Key, List);
            end if;
         end;
      end if;
   end Unbind_Visual;

   -------------------
   -- Update_Cursor --
   -------------------

   procedure Update_Cursor
     (Entity   : in out Root_Entity_Reference'Class;
      Cursor   : Cursor_Type;
      Position : Aquarius.Layout.Position)
   is
   begin
      if Bound_Entity_Map.Contains (Entity.Key) then
         declare
            List : constant List_Of_Visuals.List :=
                     Bound_Entity_Map.Element (Entity.Key);
         begin
            for Visual of List loop
               Visual.Set_Cursor (Cursor, Position);
            end loop;
         end;
      end if;
   end Update_Cursor;

end Komnenos.Entities.Visuals;
