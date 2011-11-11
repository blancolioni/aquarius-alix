with Aquarius.Programs;

package body Aquarius.Script.Library is

   Have_Standard_Library : Boolean := False;
   Standard_Library      : Script_Environment;

   type Standard_Library_Function is access
     function (Env  : Script_Environment;
               Args : Aquarius.Script.Expressions.Array_Of_Expressions)
               return Aquarius.Script.Expressions.Expression_Access;

   type Standard_Library_Object is
     new Root_Aquarius_Object with
      record
         Name    : access String;
         Execute : Standard_Library_Function;
      end record;

   overriding
   function Name (Item : Standard_Library_Object) return String;

   procedure Create_Standard_Library;

   function Flat_Image
     (Env  : Script_Environment;
      Args : Aquarius.Script.Expressions.Array_Of_Expressions)
      return Aquarius.Script.Expressions.Expression_Access;

   -----------------------------
   -- Create_Standard_Library --
   -----------------------------

   procedure Create_Standard_Library is
      It : Environment_Entry;
   begin
      It := new Standard_Library_Object'
        (Name    => new String'("flat_image"),
         Execute => Flat_Image'Access);
      Insert (Standard_Library, It);
   end Create_Standard_Library;

   -------------
   -- Execute --
   -------------

   function Execute
     (Item : not null access Root_Aquarius_Object'Class;
      Env  : Script_Environment;
      Args : Aquarius.Script.Expressions.Array_Of_Expressions)
      return Aquarius.Script.Expressions.Expression_Access
   is
   begin
      return Standard_Library_Object (Item.all).Execute (Env, Args);
   end Execute;

   ----------------
   -- Flat_Image --
   ----------------

   function Flat_Image
     (Env  : Script_Environment;
      Args : Aquarius.Script.Expressions.Array_Of_Expressions)
      return Aquarius.Script.Expressions.Expression_Access
   is
      P : constant Aquarius.Programs.Program_Tree :=
            Aquarius.Programs.Program_Tree
              (Args (Args'First).Evaluate (Env));
   begin
      return Aquarius.Script.Expressions.String_Expression
        (P.Concatenate_Children);
   end Flat_Image;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Item : Standard_Library_Object) return String
   is
   begin
      return Item.Name.all;
   end Name;

   --------------
   -- Standard --
   --------------

   function Standard return Script_Environment is
   begin
      if not Have_Standard_Library then
         Create_Standard_Library;
         Have_Standard_Library := True;
      end if;

      return Standard_Library;
   end Standard;

end Aquarius.Script.Library;
