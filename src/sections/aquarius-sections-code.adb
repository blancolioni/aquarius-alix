with Aquarius.Rendering.Sections;

package body Aquarius.Sections.Code is

   type Code_Section is
     new Root_Aquarius_Section with
      record
         Buffer : Aquarius.Buffers.Aquarius_Buffer;
      end record;

   overriding procedure On_Key
     (Section : in out Code_Section;
      Key     : Aquarius.Keys.Aquarius_Key;
      Handled : out Boolean);

   overriding procedure Set_Point
     (Section : in out Code_Section;
      Point   : Aquarius.Layout.Position);

   ----------------------
   -- New_Code_Section --
   ----------------------

   function New_Code_Section
     (Id     : String;
      Buffer : Aquarius.Buffers.Aquarius_Buffer)
      return Aquarius_Section
   is
      Code : Code_Section;
   begin
      Code.Create (Id);
      Code.Background :=
        Aquarius.Colours.White;
      Code.Buffer := Buffer;

      declare
         Result : constant Aquarius_Section :=
                    new Code_Section'(Code);
      begin
         Buffer.Set_Current_Render
           (Aquarius.Rendering.Sections.New_Section_Renderer
              (Result));
         return Result;
      end;

   end New_Code_Section;

   ------------
   -- On_Key --
   ------------

   overriding procedure On_Key
     (Section : in out Code_Section;
      Key     : Aquarius.Keys.Aquarius_Key;
      Handled : out Boolean)
   is
   begin
      Handled := Section.Buffer.On_Key (Key);
   end On_Key;

   ---------------
   -- Set_Point --
   ---------------

   overriding procedure Set_Point
     (Section : in out Code_Section;
      Point   : Aquarius.Layout.Position)
   is
   begin
      Section.Buffer.Set_Point (Point);
   end Set_Point;

end Aquarius.Sections.Code;
