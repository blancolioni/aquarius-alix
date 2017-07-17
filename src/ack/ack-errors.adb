with Aquarius.Errors;
with Aquarius.Messages.Console;
with Aquarius.Programs;

package body Ack.Errors is

   Local_Has_Errors : Boolean := False;

   function Error_Message
     (Node    : Node_Id;
      Error   : Error_Kind)
      return String;

   ------------------
   -- Clear_Errors --
   ------------------

   procedure Clear_Errors is
   begin
      Local_Has_Errors := False;
   end Clear_Errors;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message
     (Node    : Node_Id;
      Error   : Error_Kind)
      return String
   is
   begin
      case Error is
         when E_No_Error               =>
            return "no error";
         when E_Undeclared_Name        =>
            return "undeclared: "
              & To_String (Get_Name (Node));
         when E_Redefined_Name =>
            return "redefinition of "
              & Get_Description (Get_Entity (Node));
         when E_No_Child               =>
            return "child class not found";
         when E_No_Component           =>
            return "invalid prefix in selected "
              & "component";
         when E_Id_List_With_Arguments =>
            return "arguments cannot appear here";
         when E_Id_List_With_No_Type   =>
               return "declaration group "
              & "requires a type";
         when E_Id_List_With_Routine   =>
            return "routine can have only one name";
         when E_Type_Error             =>
            return "expected type derived from "
              & Get_Description
              (Get_Error_Entity (Node))
              & " but found "
              & Get_Description
                 (Get_Type (Get_Entity (Node)));
         when E_Insufficient_Arguments =>
            return "not enough arguments";
         when E_Ignored_Return_Value =>
            return "cannot ignore return value of routine";
         when E_Too_Many_Arguments =>
            return "too many arguments";
      end case;
   end Error_Message;

   ----------------
   -- Has_Errors --
   ----------------

   function Has_Errors return Boolean is
   begin
      return Local_Has_Errors;
   end Has_Errors;

   -------------------
   -- Record_Errors --
   -------------------

   procedure Record_Errors (Node : Node_Id) is

      procedure Set_Error
        (Node  : Ack.Node_Id;
         Error : Ack.Error_Kind);

      ---------------
      -- Set_Error --
      ---------------

      procedure Set_Error
        (Node  : Ack.Node_Id;
         Error : Ack.Error_Kind)
      is
         Program : constant Aquarius.Programs.Program_Tree :=
                     Get_Program (Node);
         Message : constant String := Error_Message (Node, Error);
      begin
         Local_Has_Errors := True;
         Aquarius.Errors.Error (Program, Message);
      end Set_Error;

   begin
      Scan_Errors (Node, Set_Error'Access);
   end Record_Errors;

   -------------------
   -- Report_Errors --
   -------------------

   procedure Report_Errors (Node : Node_Id) is
      use Aquarius.Messages;
      List : Message_List;
   begin
      Get_Program (Node).Get_Messages (List);
      if Message_Count (List) > 0 then
         if Highest_Level (List) >= Warning then
            Aquarius.Messages.Console.Show_Messages (List);
         end if;
      end if;
   end Report_Errors;

end Ack.Errors;