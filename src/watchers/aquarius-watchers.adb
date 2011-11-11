package body Aquarius.Watchers is

   -----------------
   -- Add_Watcher --
   -----------------

   procedure Add_Watcher
     (To_List : in out          Watcher_List;
      W       : not null access Watcher'Class;
      Context : not null access Root_Aquarius_Object'Class)
   is
      use Watcher_Container;
      It : Cursor := To_List.Watchers.First;
   begin
      while Has_Element (It) and then
        (Element (It).Watcher_Object /= W or else
           Element (It).Context /= Context)
      loop
         Next (It);
      end loop;

      if not Has_Element (It) then
         To_List.Watchers.Append ((W, Context));
      end if;

   end Add_Watcher;

   -------------------
   -- Signal_Change --
   -------------------

   procedure Signal_Change (Item     : access Root_Aquarius_Object'Class;
                            Watchers : in out Watcher_List)
   is
      use Watcher_Container;
      It : Cursor := Watchers.Watchers.First;
   begin
      while Has_Element (It) loop
         Element (It).Watcher_Object.Object_Changed
           (Item, Element (It).Context);
         Next (It);
      end loop;
   end Signal_Change;

end Aquarius.Watchers;
