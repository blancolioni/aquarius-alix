package body Aquarius.Paths is

   Unix_Paths : constant Boolean := True;
   Separator  : constant Character := '/';

   ----------------
   -- Join_Paths --
   ----------------

   function Join_Paths (Left, Right : Aquarius_Path) return Aquarius_Path is
      use type Ada.Containers.Count_Type;
      Result : Aquarius_Path := Left;
   begin
      if Right.Absolute then
         return Right;
      end if;

      for I in 1 .. Right.Elements.Last_Index loop
         declare
            Elem : constant Unbounded_String := Right.Elements.Element (I);
         begin
            if Elem = "." then
               null;
            elsif Elem = ".." then
               if Result.Elements.Length > 0 then
                  Result.Elements.Delete (Left.Elements.Last_Index);
               else
                  Result.Elements.Append (Elem);
               end if;
            else
               Result.Elements.Append (Right.Elements.Element (I));
            end if;
         end;
      end loop;
      return Result;
   end Join_Paths;

   ----------------------
   -- To_Aquarius_Path --
   ----------------------

   function To_Aquarius_Path (OS_Path : String) return Aquarius_Path is
      Result : Aquarius_Path;
      Start  : Positive       := OS_Path'First;
      Index  : Positive;
   begin
      if not Unix_Paths and then
        OS_Path'Length > 1 and then
        OS_Path (OS_Path'First + 1) = ':'
      then
         declare
            Prefix : constant String :=
                       OS_Path (OS_Path'First .. OS_Path'First + 1);
         begin
            Result.Prefix := To_Unbounded_String (Prefix);
            Start := OS_Path'First + 2;
         end;
      end if;

      Result.Absolute := OS_Path (Start) = Separator;

      if Result.Absolute then
         Start := Start + 1;
      end if;

      while Start < OS_Path'Last loop

         while Start <= OS_Path'Last
           and then OS_Path (Start) = Separator
         loop
            Start := Start + 1;
         end loop;

         exit when Start > OS_Path'Last;

         Index := Start;
         while Index <= OS_Path'Last
           and then OS_Path (Index) /= Separator
         loop
            Index := Index + 1;
         end loop;

         if Index > Start then
            Result.Elements.Append
              (To_Unbounded_String (OS_Path (Start .. Index - 1)));
            Start := Index;
         else
            exit;
         end if;

      end loop;

      return Result;

   end To_Aquarius_Path;

   ----------------
   -- To_OS_Path --
   ----------------

   function To_OS_Path (Path : Aquarius_Path) return String is
      Result : Unbounded_String;
   begin
      if Path.Absolute then
         Result := Path.Prefix & Separator;
      end if;

      for I in 1 .. Path.Elements.Last_Index loop
         Result := Result & Path.Elements.Element (I);
         if I < Path.Elements.Last_Index then
            Result := Result & Separator;
         end if;
      end loop;

      return To_String (Result);

   end To_OS_Path;

end Aquarius.Paths;
