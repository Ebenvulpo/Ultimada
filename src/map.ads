with Ada.Finalization;
with Interfaces.C;     use Interfaces.C;
with Tile;             use Tile;
with Video;            use Video;

package Map is
   package C renames Interfaces.C;

   type Map_Type is new Ada.Finalization.Controlled with private;

   subtype Map_Width  is C.int range 0 .. 255;
   subtype Map_Height is C.int range 0 .. 255;

   procedure Create (Map : in out Map_Type);
   procedure Render (Map : in out Map_Type; Video : in out Video_Driver);

private
   type Tile_Array        is array (C.int range <>) of Tile_Type;
   type Tile_Array_Access is access Tile_Array;

   type Map_Type is new Ada.Finalization.Controlled with
      record
	 Tiles : Tile_Array_Access := null;
      end record;

   overriding procedure Finalize (Map : in out Map_Type);
end Map;
