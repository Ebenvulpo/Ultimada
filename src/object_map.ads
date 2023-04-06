with Interfaces.C; use Interfaces.C;
with Map;
with Object;       use Object;
with Video;        use Video;

package Object_Map is
   package C renames Interfaces.C;

   type Object_Map_Type is tagged limited private;

   subtype Object_Map_Width  is Map.Map_Width_Type;
   subtype Object_Map_Height is Map.Map_Height_Type;

   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize (Object_Map : in out Object_Map_Type);
   procedure Initialize
     (Object_Map   : in out Object_Map_Type;
      Low_Tile     : in     Integer;
      High_Tile    : in     Integer;
      Spawn_Rate   : in     Float);

   ------------------------------
   --  Object_Map Subprograms  --
   ------------------------------
   procedure Render
     (Object_Map     : in out Object_Map_Type;
      Video          : in out Video_Driver;
      Offset_X       : in     C.int;
      Offset_Y       : in     C.int;
      Pixel_Offset_X : in     C.int;
      Pixel_Offset_Y : in     C.int);

private
   X_First : constant C.int := Object_Map_Width'First;
   X_Last  : constant C.int := Object_Map_Width'Last;
   Y_First : constant C.int := Object_Map_Height'First;
   Y_Last  : constant C.int := Object_Map_Height'Last;

   type Object_Map_X is array (C.int range X_First .. X_Last) of Object_Type;
   type Object_Map_Y is array (C.int range Y_First .. Y_Last) of Object_Map_X;

   type Object_Map_Access is access Object_Map_Y;

   type Object_Map_Type is tagged limited
      record
	 Objects : Object_Map_Access := null;
      end record;
end Object_Map;
