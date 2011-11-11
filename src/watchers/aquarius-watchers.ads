private with Ada.Containers.Doubly_Linked_Lists;

package Aquarius.Watchers is

   type Watcher_List is private;

   procedure Add_Watcher
     (To_List : in out          Watcher_List;
      W       : not null access Watcher'Class;
      Context : not null access Root_Aquarius_Object'Class);

   procedure Signal_Change (Item     : access Root_Aquarius_Object'Class;
                            Watchers : in out Watcher_List);

private

   type Watcher_Entry is
      record
         Watcher_Object : access Watcher'Class;
         Context        : access Root_Aquarius_Object'Class;
      end record;

   package Watcher_Container is
      new Ada.Containers.Doubly_Linked_Lists (Watcher_Entry);

   type Watcher_List is
      record
         Watchers : Watcher_Container.List;
      end record;

end Aquarius.Watchers;
