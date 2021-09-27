with Ada.Strings.Unbounded;

with Aquarius.Grammars;
with Aquarius.Interaction;
with Aquarius.Layout;
with Aquarius.Messages;
with Aquarius.Programs;
with Aquarius.Programs.Editor;
with Aquarius.Programs.Parser;
with Aquarius.Rendering;
with Aquarius.Trees;
with Aquarius.Trees.Cursors;
with Aquarius.UI;

private with Aquarius.Source;

package Aquarius.Buffers is

   type Aquarius_Buffer_Record is
     new Root_Aquarius_Object
     and Aquarius.Programs.Editor.Aquarius_Rendering_Interface
     and Aquarius.Messages.Message_Location
     and Aquarius.Interaction.Interactor
     with private;

   type Aquarius_Buffer is access all Aquarius_Buffer_Record'Class;

   type Buffer_View is interface;

   function New_Buffer_From_File
     (UI   : Aquarius.UI.Aquarius_UI;
      Path : String)
      return Aquarius_Buffer;
   --  Create a buffer for the given file with a default project.
   --  The file is not loaded.

   function New_Buffer_From_File
     (UI   : Aquarius.UI.Aquarius_UI;
      Path    : String;
      Store   : not null access Programs.Root_Program_Tree_Store'Class)
     return Aquarius_Buffer;
   --  Create a buffer for the given file using the given project.
   --  The file is not loaded.

   procedure Load
     (Buffer      : Aquarius_Buffer;
      Synchronous : Boolean);

   function Load_Buffer_From_File
     (UI   : Aquarius.UI.Aquarius_UI;
      Path : String)
     return Aquarius_Buffer;

   function New_Empty_Buffer
     (UI   : Aquarius.UI.Aquarius_UI;
      Grammar_Name : String)
      return Aquarius_Buffer;

   function New_Buffer_From_Tree
     (UI   : Aquarius.UI.Aquarius_UI;
      Name    : in     String;
      Program : not null access Programs.Program_Tree_Type'Class)
      return Aquarius_Buffer;

   overriding
   function Name (Buffer : Aquarius_Buffer_Record)
                 return String;

   function Full_Path
     (Buffer : not null access Aquarius_Buffer_Record'Class)
     return String;

   function File_Simple_Name
     (Buffer : not null access Aquarius_Buffer_Record'Class)
     return String;

   not overriding
   procedure Render
     (Buffer  : not null access Aquarius_Buffer_Record);

   not overriding
   procedure Render
     (Buffer  : not null access Aquarius_Buffer_Record;
      Display : in out Aquarius.Rendering.Root_Aquarius_Renderer'Class);

   --  Implementation of message interface

   --  A buffer "contains" all the messages of its internal program tree

   --  Show_Location: name of the buffer
--     function Show_Location
--       (Location : Aquarius_Buffer_Record)
--       return String;

   overriding
   function Location_Name (Location : Aquarius_Buffer_Record)
                          return String;

   overriding
   function Location_Line (Location : Aquarius_Buffer_Record)
                          return Natural;

   overriding
   function Location_Column (Location : Aquarius_Buffer_Record)
                            return Natural;
   --  Attach_Message: add a buffer-level message
   overriding
   procedure Attach_Message (To    : in out Aquarius_Buffer_Record;
                             Item  : Aquarius.Messages.Message);

   overriding
   function Before
     (Left   : Aquarius_Buffer_Record;
      Right  : not null access Aquarius.Messages.Message_Location'Class)
      return Boolean;

   overriding
   procedure Get_Messages (From  : in     Aquarius_Buffer_Record;
                           List  : in out Aquarius.Messages.Message_List);

   overriding
   procedure Clear_Messages (Item : in out Aquarius_Buffer_Record);

   overriding
   procedure Update
     (Item  : in out Aquarius_Buffer_Record;
      Start : not null access Aquarius.Trees.Root_Tree_Type'Class);

   --  Implementation of editor rendering interface
   overriding procedure Update
     (Buffer  : in out Aquarius_Buffer_Record;
      Point   : Aquarius.Trees.Cursors.Cursor;
      Partial : String);

   --  Basic buffer operations
   procedure Set_Point (Buffer : not null access Aquarius_Buffer_Record'Class;
                        Point  : in     Aquarius.Layout.Position);

   function Program (Buffer : Aquarius_Buffer_Record'Class)
                    return Aquarius.Programs.Program_Tree;

   function Grammar (Buffer : Aquarius_Buffer_Record'Class)
                     return Aquarius.Grammars.Aquarius_Grammar;

private

   type Aquarius_Buffer_Record is
     new Root_Aquarius_Object
     and Aquarius.Programs.Editor.Aquarius_Rendering_Interface
     and Aquarius.Messages.Message_Location
     and Aquarius.Interaction.Interactor with
      record
         Buffer_UI      : Aquarius.UI.Aquarius_UI;
         Editor         : Aquarius.Programs.Editor.Program_Editor;
         File_Buffer    : Boolean;
         Grammar        : Aquarius.Grammars.Aquarius_Grammar;
         Program_Store  : Aquarius.Programs.Program_Tree_Store;
         Buffer_Name    : Ada.Strings.Unbounded.Unbounded_String;
         Full_Path      : Ada.Strings.Unbounded.Unbounded_String;
         Contents       : Aquarius.Programs.Program_Tree;
         Local_Messages : Aquarius.Messages.Message_List;
         Point_Position : Aquarius.Layout.Position;
         Parsing        : Aquarius.Programs.Parser.Parse_Context;
         Node_Offset    : Aquarius.Layout.Count;
         Buffer_File    : Aquarius.Source.Source_File;
         File_Position  : Aquarius.Source.Source_Position;
         Rendering      : Boolean := False;
      end record;

end Aquarius.Buffers;
