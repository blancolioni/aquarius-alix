with Aquarius.Programs;

with Aquarius.Actions.Interpreter;
with Aquarius.Grammars.Manager;
with Aquarius.Loader;

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
         Name : Ada.Strings.Unbounded.Unbounded_String;
         Execute : Standard_Library_Function;
      end record;

   overriding
   function Name (Item : Standard_Library_Object) return String;

   procedure Create_Standard_Library;

   function Flat_Image
     (Env  : Script_Environment;
      Args : Aquarius.Script.Expressions.Array_Of_Expressions)
      return Aquarius.Script.Expressions.Expression_Access;

   function External
     (Env  : Script_Environment;
      Args : Aquarius.Script.Expressions.Array_Of_Expressions)
      return Aquarius.Script.Expressions.Expression_Access;

   -----------------------------
   -- Create_Standard_Library --
   -----------------------------

   procedure Create_Standard_Library is
      use Ada.Strings.Unbounded;
   begin
      Insert (Standard_Library, "flat_image",
              (Aquarius_Object_Entry,
               new Standard_Library_Object'
                 (Name    => To_Unbounded_String ("flat_image"),
                  Execute => Flat_Image'Access)));
      Insert (Standard_Library, "external",
              (Aquarius_Object_Entry,
               new Standard_Library_Object'
                 (Name    => To_Unbounded_String ("external"),
                  Execute => External'Access)));
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

   --------------
   -- External --
   --------------

   function External
     (Env  : Script_Environment;
      Args : Aquarius.Script.Expressions.Array_Of_Expressions)
      return Aquarius.Script.Expressions.Expression_Access
   is
      use Ada.Strings.Unbounded;
      Path : constant String :=
               To_String (Find (Env, "path").String_Value)
               & "/"
               & Args (Args'First).Name
               & ".action";
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                  Aquarius.Grammars.Manager.Get_Grammar_For_File (Path);
      Action : constant Aquarius.Programs.Program_Tree :=
                  Aquarius.Loader.Load_From_File
                    (Grammar => Grammar,
                     Path    => Path);
      Program : constant Aquarius.Programs.Program_Tree :=
                  Aquarius.Programs.Program_Tree
                    (Find (Env, "node").Object_Value);
   begin
      Aquarius.Actions.Interpreter.Interpret_Action
        (Action => Action,
         Target => Program);
      return Args (Args'First);
   end External;

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
      return Ada.Strings.Unbounded.To_String (Item.Name);
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
