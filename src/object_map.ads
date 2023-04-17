with Map;
with Object;       use Object;
with SDL2_Stdinc;  use SDL2_Stdinc;
with Video;        use Video;

package Object_Map is
   type Object_Map_Type is tagged limited private;

   subtype Object_Map_Width  is Map.Map_Width_Type;
   subtype Object_Map_Height is Map.Map_Height_Type;

   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize (Object_Map : in out Object_Map_Type);
   procedure Initialize   (Object_Map : in out Object_Map_Type);

   ------------------------------
   --  Object_Map Subprograms  --
   ------------------------------
   procedure Change_Object
     (Object_Map : in out Object_Map_Type;
      Item       : in     Object.Item_Type;
      X          : in     Object_Map_Width;
      Y          : in     Object_Map_Height);
   
   function Get_Item
     (Object_Map : in Object_Map_Type;
      X          : in Object_Map_Width;
      Y          : in Object_Map_Height)
     return Object.Item_Type;

   procedure Render
     (Object_Map     : in out Object_Map_Type;
      Video          : in out Video_Driver;
      Offset_X       : in     Sint32;
      Offset_Y       : in     Sint32;
      Pixel_Offset_X : in     Sint32;
      Pixel_Offset_Y : in     Sint32);

private
   X_First : constant Sint32 := Object_Map_Width'First;
   X_Last  : constant Sint32 := Object_Map_Width'Last;
   Y_First : constant Sint32 := Object_Map_Height'First;
   Y_Last  : constant Sint32 := Object_Map_Height'Last;

   type Object_Map_X is array (Sint32 range X_First .. X_Last) of Object_Type;
   type Object_Map_Y is array (Sint32 range Y_First .. Y_Last) of Object_Map_X;

   type Object_Map_Access is access Object_Map_Y;

   type Object_Map_Type is tagged limited
      record
	 Objects : Object_Map_Access := null;
      end record;
end Object_Map;
