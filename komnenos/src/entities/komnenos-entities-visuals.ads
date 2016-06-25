with Aquarius.Layout;

package Komnenos.Entities.Visuals is

   procedure Bind_Visual
     (Visual : not null access Entity_Visual'Class;
      Entity : not null access Root_Entity_Reference'Class);

   procedure Unbind_Visual
     (Visual : not null access Entity_Visual'Class);

   procedure Invalidate_Visuals
     (Entity : in out Root_Entity_Reference'Class);

   procedure Update_Cursor
     (Entity : in out Root_Entity_Reference'Class;
      Position : Aquarius.Layout.Position);

   procedure Insert_At_Cursor
     (Entity : in out Root_Entity_Reference'Class;
      Text   : String);

end Komnenos.Entities.Visuals;
