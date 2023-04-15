with Interfaces.C; use Interfaces.C;
with Tile;         use Tile;
with Video;        use Video;

package Map is
   package C renames Interfaces.C;

   type Map_Type is tagged private;

   subtype Map_Width_Type  is C.int range 0 .. 256;
   subtype Map_Height_Type is C.int range 0 .. 256;

   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize (Map : in out Map_Type);
   procedure Initialize
     (Map          : in out Map_Type;
      low_tile     : in     Integer;
      high_tile    : in     Integer;
      default_tile : in     Integer;
      spawn_rate   : in     Float);

   -----------------------
   --  Map Subprograms  --
   -----------------------
   function Get_Tile
     (Map : in out Map_Type;
      X   : in     Map_Width_Type;
      Y   : in     Map_Height_Type)
     return Tile_Type;
   
   function Is_Tile_Walkable
     (Map : in out Map_Type;
      X   : in     Map_Width_Type'Base;
      Y   : in     Map_Height_Type'Base)
     return Boolean;

   procedure Render
     (Map            : in out Map_Type;
      Video          : in out Video_Driver;
      Offset_X       : in     C.int;
      Offset_Y       : in     C.int;
      Pixel_Offset_X : in     C.int;
      Pixel_Offset_Y : in     C.int);

private
   type Tile_Row is array (C.int range 0 .. Map_Width_Type'Last)  of Tile_Type;
   type Tile_Map is array (C.int range 0 .. Map_Height_Type'Last) of Tile_Row;

   type Tile_Map_Access is access Tile_Map;

   type Map_Type is tagged
      record
	 Tiles  : Tile_Map_Access := null;
      end record;
   
   procedure Generate_Grass      (Map : in out Map_Type);
   procedure Generate_Forests    (Map : in out Map_Type);
   procedure Generate_Lava_Pools (Map : in out Map_Type);
   procedure Generate_Poles      (Map : in out Map_Type);

   procedure Trees (Map : in out Map_Type);
end Map;
