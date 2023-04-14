with As.Environment;

package body As.Expressions is

   ----------------------
   -- Current_Location --
   ----------------------

   function Current_Location
     return Reference
   is
   begin
      return new Instance'
        (Class       => Word_Node,
         Current_Loc => True,
         Word_Value  => 0);
   end Current_Location;

   ---------------------------
   -- Get_Instruction_Value --
   ---------------------------

   function Get_Instruction_Value
     (This : Instance;
      Env  : not null access constant As.Environment.Instance'Class)
      return As.Instructions.Reference
   is
   begin
      if This.Class = Instruction_Node then
         return This.Instr_Value;
      else
         return Env.Get_Value (As.Names."-" (This.Id_Value))
           .Get_Instruction_Value (Env);
      end if;
   end Get_Instruction_Value;

   ------------------------
   -- Get_Register_Value --
   ------------------------

   function Get_Register_Value
     (This : Instance;
      Env  : not null access constant As.Environment.Instance'Class)
      return Register_Index
   is
   begin
      if This.Class = Register_Node then
         return This.Reg_Value;
      else
         return Env.Get_Value (As.Names."-" (This.Id_Value))
           .Get_Register_Value (Env);
      end if;
   end Get_Register_Value;

   ----------------------
   -- Get_Static_Value --
   ----------------------

   function Get_Static_Value
     (This : Instance)
      return Word_32
   is
   begin
      case This.Class is
         when Word_Node =>
            return This.Word_Value;
         when Operator_Node =>
            declare
               Left   : constant Word_32 :=
                          (if This.Left = null
                           then 0
                           else This.Left.Get_Static_Value);
               Right  : constant Word_32 := This.Right.Get_Static_Value;
            begin
               case This.Op is
                  when Op_Plus =>
                     return Left + Right;
                  when Op_Minus =>
                     return (if This.Right = null
                             then (not Left) + 1
                             else Left - Right);
                  when Op_Multiply =>
                     return Left * Right;
                  when Op_Divide =>
                     return (if Right = 0 then 0 else Left / Right);
               end case;
            end;
         when others =>
            return (raise Constraint_Error with
                    This.Class'Image & " does not have a static value");
      end case;
   end Get_Static_Value;

   --------------------
   -- Get_Word_Value --
   --------------------

   function Get_Word_Value
     (This : Instance;
      Env  : not null access constant As.Environment.Instance'Class)
      return Word_32
   is
   begin
      case This.Class is
         when Word_Node =>
            if This.Current_Loc then
               return Env.Get_Location;
            else
               return This.Word_Value;
            end if;
         when Local_Node =>
            return Env.Find_Local_Label (This.Label_Index, This.Forward);
         when Identifier_Node =>
            return Env.Get_Value (As.Names."-" (This.Id_Value))
              .Get_Word_Value (Env);
         when Operator_Node =>
            declare
               Left : constant Word_32 :=
                         (if This.Left = null
                          then 0
                          else This.Left.Get_Word_Value (Env));
               Right  : constant Word_32 := This.Right.Get_Word_Value (Env);
            begin
               case This.Op is
                  when Op_Plus =>
                     return Left + Right;
                  when Op_Minus =>
                     return (if This.Right = null
                             then (not Left) + 1
                             else Left - Right);
                  when Op_Multiply =>
                     return Left * Right;
                  when Op_Divide =>
                     return (if Right = 0 then 0 else Left / Right);
               end case;
            end;
         when others =>
            return (raise Constraint_Error with
                    This.Class'Image & " does not have a word value");
      end case;
   end Get_Word_Value;

   ---------------------------
   -- Has_Instruction_Value --
   ---------------------------

   function Has_Instruction_Value
     (This : Instance;
      Env  : not null access constant As.Environment.Instance'Class)
      return Boolean
   is
   begin
      case This.Class is
         when Word_Node =>
            return False;
         when Register_Node =>
            return False;
         when Instruction_Node =>
            return True;
         when Local_Node =>
            return False;
         when Identifier_Node =>
            declare
               Id : constant String := As.Names."-" (This.Id_Value);
            begin
               return Env.Contains (Id)
                 and then Env.Has_Value (Id)
                 and then Env.Get_Value (Id).Has_Instruction_Value (Env);
            end;
         when Operator_Node =>
            return False;
      end case;
   end Has_Instruction_Value;

   ------------------------
   -- Has_Register_Value --
   ------------------------

   function Has_Register_Value
     (This : Instance;
      Env  : not null access constant As.Environment.Instance'Class)
      return Boolean
   is
   begin
      case This.Class is
         when Word_Node =>
            return False;
         when Register_Node =>
            return True;
         when Instruction_Node =>
            return False;
         when Local_Node =>
            return False;
         when Identifier_Node =>
            declare
               Id : constant String := As.Names."-" (This.Id_Value);
            begin
               return Env.Contains (Id)
                 and then Env.Has_Value (Id)
                 and then Env.Get_Value (Id).Has_Register_Value (Env);
            end;
         when Operator_Node =>
            return False;
      end case;
   end Has_Register_Value;

   ----------------------
   -- Has_Static_Value --
   ----------------------

   function Has_Static_Value
     (This : Instance)
      return Boolean
   is
   begin
      case This.Class is
         when Word_Node =>
            return not This.Current_Loc;
         when Register_Node =>
            return False;
         when Instruction_Node =>
            return False;
         when Local_Node =>
            return False;
         when Identifier_Node =>
            return False;
         when Operator_Node =>
            return This.Right.Has_Static_Value
              and then (This.Left = null or else This.Left.Has_Static_Value);
      end case;
   end Has_Static_Value;

   ---------------
   -- Has_Value --
   ---------------

   function Has_Value
     (This : Instance;
      Env  : not null access constant As.Environment.Instance'Class)
      return Boolean
   is
   begin
      case This.Class is
         when Word_Node =>
            return True;
         when Register_Node =>
            return True;
         when Instruction_Node =>
            return True;
         when Local_Node =>
            return Env.Contains_Local_Label (This.Label_Index, This.Forward);
         when Identifier_Node =>
            declare
               Id : constant String := As.Names."-" (This.Id_Value);
            begin
               return Env.Contains (Id)
                 and then Env.Has_Value (Id)
                 and then Env.Get_Value (Id).Has_Value (Env);
            end;
         when Operator_Node =>
            return This.Right.Has_Value (Env)
              and then (This.Left = null or else This.Left.Has_Value (Env));
      end case;
   end Has_Value;

   --------------------
   -- Has_Word_Value --
   --------------------

   function Has_Word_Value
     (This : Instance;
      Env  : not null access constant As.Environment.Instance'Class)
      return Boolean
   is
   begin
      case This.Class is
         when Word_Node =>
            return True;
         when Register_Node =>
            return False;
         when Instruction_Node =>
            return False;
         when Local_Node =>
            return Env.Contains_Local_Label (This.Label_Index, This.Forward);
         when Identifier_Node =>
            declare
               Id : constant String := As.Names."-" (This.Id_Value);
            begin
               return Env.Contains (Id)
                 and then Env.Has_Value (Id)
                 and then Env.Get_Value (Id).Has_Word_Value (Env);
            end;
         when Operator_Node =>
            return This.Right.Has_Word_Value (Env)
              and then (This.Left = null
                        or else This.Left.Has_Word_Value (Env));
      end case;
   end Has_Word_Value;

   ----------------
   -- Identifier --
   ----------------

   function Identifier (Name : String) return Reference is
   begin
      return new Instance'
        (Class     => Identifier_Node,
         Id_Value  => As.Names."+" (Name));
   end Identifier;

   -----------------
   -- Instruction --
   -----------------

   function Instruction
     (Value : As.Instructions.Reference)
      return Reference
   is
   begin
      return new Instance'
        (Class => Instruction_Node,
         Instr_Value => Value);
   end Instruction;

   -----------
   -- Local --
   -----------

   function Local
     (Index   : Positive;
      Forward : Boolean)
      return Reference
   is
   begin
      return new Instance'
        (Class => Local_Node, Label_Index => Index, Forward => Forward);
   end Local;

   -------------
   -- Operate --
   -------------

   function Operate
     (Operator    : Operator_Type;
      Left, Right : Reference)
      return Reference
   is
   begin
      if Right = null then
         return new Instance'
           (Class => Operator_Node, Op => Operator,
            Left  => null, Right => Left);
      else
         return new Instance'
           (Class => Operator_Node, Op => Operator,
            Left  => Left, Right => Right);
      end if;
   end Operate;

   --------------
   -- Register --
   --------------

   function Register (Value : Register_Index) return Reference is
   begin
      return new Instance'
        (Class     => Register_Node,
         Reg_Value => Value);
   end Register;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Instance) return String is
   begin
      case This.Class is
         when Word_Node =>
            return This.Word_Value'Image;
         when Identifier_Node =>
            return As.Names."-" (This.Id_Value);
         when Register_Node =>
            declare
               Img : String := This.Reg_Value'Image;
            begin
               Img (Img'First) := '%';
               return Img;
            end;
         when Instruction_Node =>
            return This.Instr_Value.To_String;
         when Local_Node =>
            declare
               Img : constant String := This.Label_Index'Image;
            begin
               return Img (2 .. Img'Last)
                 & (if This.Forward then 'F' else 'B');
            end;

         when Operator_Node =>
            if This.Left = null then
               return Operator_Symbol (This.Op)
                 & This.Right.To_String;
            else
               return "(" & This.Left.To_String
                 & " " & Operator_Symbol (This.Op)
                 & " " & This.Right.To_String
                 & ")";
            end if;
      end case;
   end To_String;

   ----------------
   -- Word_Value --
   ----------------

   function Word_Value (Value : Word_32) return Reference is
   begin
      return new Instance'
        (Class       => Word_Node,
         Current_Loc => False,
         Word_Value  => Value);
   end Word_Value;

end As.Expressions;
