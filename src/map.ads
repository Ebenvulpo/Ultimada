with Ada.Finalization;
with Interfaces.C;     use Interfaces.C;
with Tile;             use Tile;
with Video;            use Video;

package Map is
   package C renames Interfaces.C;

   type Map_Type is new Ada.Finalization.Controlled with private;

   subtype Map_Width_Type  is C.int range 0 .. 256;
   subtype Map_Height_Type is C.int range 0 .. 256;

   procedure Create (Map : in out Map_Type);
   procedure Render
     (Map      : in out Map_Type;
      Video    : in out Video_Driver;
      Offset_X : in     C.int;
      Offset_Y : in     C.int);

private
   type Tile_Row is array (C.int range 0 .. Map_Width_Type'Last) of Tile_Type;
   type Tile_Map is array (C.int range 0 .. Map_Height_Type'Last) of Tile_Row;

   type Tile_Map_Access is access Tile_Map;

   type Map_Type is new Ada.Finalization.Controlled with
      record
	 Tiles  : Tile_Map_Access := null;
      end record;

   overriding procedure Finalize (Map : in out Map_Type);
end Map;
