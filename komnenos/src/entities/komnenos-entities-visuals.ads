with Aquarius.Layout;

package Komnenos.Entities.Visuals is

   procedure Bind_Visual
     (Visual : not null access Entity_Visual'Class;
      Entity : not null access Root_Entity_Reference'Class);

   procedure Unbind_Visual
     (Visual : not null access Entity_Visual'Class);

   procedure Invalidate_Visuals
     (Entity : not null access Root_Entity_Reference'Class);

   procedure Update_Cursor
     (Entity : not null access Root_Entity_Reference'Class;
      Position : Aquarius.Layout.Position);

end Komnenos.Entities.Visuals;
