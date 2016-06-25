with Aquarius.Layout;

package Komnenos.Commands.Cursor_Movement is

   function Move_By_Line_Command
     (Offset : Aquarius.Layout.Line_Offset)
      return Root_Komnenos_Command'Class;

   function Move_By_Character_Command
     (Offset : Aquarius.Layout.Character_Offset)
      return Root_Komnenos_Command'Class;

   function Move_To_Position_Command
     (New_Position : Aquarius.Layout.Position)
      return Root_Komnenos_Command'Class;

end Komnenos.Commands.Cursor_Movement;
