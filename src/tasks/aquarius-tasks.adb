with Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Vectors;
with Ada.Unchecked_Deallocation;

package body Aquarius.Tasks is

   package Change_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Task_Access is access all Root_Task_Type'Class;

   package Task_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Task_Access);

   protected Task_Manager is
      function Num_Tasks return Natural;
      function Stopping return Boolean;
      procedure Add_Task (Item : Task_Access);
      entry Next_Task (Item : out Task_Access);
      procedure Stop;
      procedure Set_Changed (Name : String);
      procedure Clear_Changed (Name : String);
      function Changed (Name : String) return Boolean;
   private
      Tasks         : Task_Lists.List;
      Stopping_Flag : Boolean := False;
      Changes       : Change_Vectors.Vector;
   end Task_Manager;

   task Task_Execution;

   --------------
   -- Add_Task --
   --------------

   procedure Add_Task
     (Item : Root_Task_Type'Class)
   is
      T : constant Task_Access :=
            new Root_Task_Type'Class'(Item);
   begin
      Task_Manager.Add_Task (T);
   end Add_Task;

   -------------
   -- Changed --
   -------------

   function Changed (Name : String) return Boolean is
   begin
      return Task_Manager.Changed (Name);
   end Changed;

   -------------------
   -- Clear_Changed --
   -------------------

   procedure Clear_Changed (Name : String) is
   begin
      Task_Manager.Clear_Changed (Name);
   end Clear_Changed;

   -----------------
   -- Set_Changed --
   -----------------

   procedure Set_Changed (Name : String) is
   begin
      Task_Manager.Set_Changed (Name);
   end Set_Changed;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Task_Manager.Stop;
   end Stop;

   --------------------
   -- Task_Execution --
   --------------------

   task body Task_Execution is
      Current : Task_Access;
      procedure Free is
         new Ada.Unchecked_Deallocation (Root_Task_Type'Class, Task_Access);
   begin
      loop
         Task_Manager.Next_Task (Current);
         exit when Current = null;
         begin
            Current.Execute;
         exception
            when others =>
               Ada.Text_IO.Put_Line
                 ("task terminated abnormally");
         end;
         Free (Current);
      end loop;
   end Task_Execution;

   ------------------
   -- Task_Manager --
   ------------------

   protected body Task_Manager is

      --------------
      -- Add_Task --
      --------------

      procedure Add_Task (Item : Task_Access) is
      begin
         Tasks.Append (Item);
      end Add_Task;

      -------------
      -- Changed --
      -------------

      function Changed (Name : String) return Boolean is
      begin
         for I in 1 .. Changes.Last_Index loop
            if Changes.Element (I) = Name then
               return True;
            end if;
         end loop;
         return False;
      end Changed;

      -------------------
      -- Clear_Changed --
      -------------------

      procedure Clear_Changed (Name : String) is
      begin
         for I in 1 .. Changes.Last_Index loop
            if Changes.Element (I) = Name then
               Changes.Delete (I);
               return;
            end if;
         end loop;
      end Clear_Changed;

      ---------------
      -- Next_Task --
      ---------------

      entry Next_Task (Item : out Task_Access)
        when Num_Tasks > 0 or else Stopping
      is
      begin
         if Stopping_Flag then
            Item := null;
         else
            Item := Tasks.First_Element;
            Tasks.Delete_First;
         end if;
      end Next_Task;

      ---------------
      -- Num_Tasks --
      ---------------

      function Num_Tasks return Natural is
      begin
         return Natural (Tasks.Length);
      end Num_Tasks;

      -----------------
      -- Set_Changed --
      -----------------

      procedure Set_Changed (Name : String) is
      begin
         for I in 1 .. Changes.Last_Index loop
            if Changes.Element (I) = Name then
               return;
            end if;
         end loop;
         Changes.Append (Name);
      end Set_Changed;

      ----------
      -- Stop --
      ----------

      procedure Stop is
      begin
         Stopping_Flag := True;
      end Stop;

      --------------
      -- Stopping --
      --------------

      function Stopping return Boolean is
      begin
         return Stopping_Flag;
      end Stopping;

   end Task_Manager;

end Aquarius.Tasks;
