with Komnenos.Entities;

package Komnenos.Commands is

   type Root_Komnenos_Command is abstract tagged private;

   procedure Set_Entity
     (Command : in out Root_Komnenos_Command'Class;
      Entity  : Komnenos.Entities.Entity_Reference);

   procedure Execute (Command : in out Root_Komnenos_Command)
   is abstract;

   procedure Undo (Command : in out Root_Komnenos_Command) is abstract;

   function Show (Command : Root_Komnenos_Command) return String
   is ("[cmd]");

   function Can_Undo (Command : Root_Komnenos_Command) return Boolean
   is (True);

   function Is_Change (Command : Root_Komnenos_Command) return Boolean
   is (True);

   type Komnenos_Command is access all Root_Komnenos_Command'Class;

--     type Komnenos_Command_Type is
--       (No_Command,
--        Move_Cursor_Command,
--        Set_Cursor_Command,
--        Insert_Character_Command,
--        Delete_Command,
--        New_Line_Command);
--
--     type Move_Unit_Type is
--       (By_Character,
--        By_Token,
--        By_Line,
--        By_Fragment);
--
--     type Komnenos_Command (Command : Komnenos_Command_Type) is
--        record
--           case Command is
--              when No_Command =>
--                 null;
--              when Move_Cursor_Command =>
--                 Offset : Integer;
--                 Units  : Move_Unit_Type;
--              when Set_Cursor_Command =>
--                 New_Position : Aquarius.Layout.Position;
--              when Insert_Character_Command =>
--                 Ch           : Character;
--              when Delete_Command =>
--                 Delete_Start : Natural;
--                 Delete_End   : Natural;
--              when New_Line_Command =>
--                 null;
--           end case;
--        end record;

--     function Show (Command : Komnenos_Command) return String;
--
--     type Command_Reference is private;

private

   type Root_Komnenos_Command is abstract tagged
      record
         Entity : Komnenos.Entities.Entity_Reference;
      end record;

end Komnenos.Commands;
