package Tile is
   type    Tile_Type    is tagged private;
   subtype Tile_ID_Type is Natural;

   procedure Create (Tile: out Tile_Type; ID : in Tile_ID_Type := 0);

   function Get_ID (Tile : in Tile_Type) return Tile_ID_Type;

private
   type Tile_Type is tagged
      record
	 ID : Tile_ID_Type;
      end record;
end Tile;
