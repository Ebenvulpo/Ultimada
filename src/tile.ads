package Tile is
   type    Tile_Type    is tagged private;
   subtype Tile_ID_Type is Natural;

   -----------------
   --  Constants  --
   -----------------
   Floor_Dirt   : constant Tile_ID_Type := 0;
   Floor_Grass  : constant Tile_ID_Type := 1;
   Floor_Grass2 : constant Tile_ID_Type := 2;
   Floor_Grass3 : constant Tile_ID_Type := 3;
   Floor_Ice    : constant Tile_ID_Type := 4;
   Floor_Lava   : constant Tile_ID_Type := 5;
   Floor_Lava2  : constant Tile_ID_Type := 6;
   Floor_Lava3  : constant Tile_ID_Type := 7;
   Rock_Pink    : constant Tile_ID_Type := 8;
   Tree         : constant Tile_ID_Type := 9;
   Tree_Pink    : constant Tile_ID_Type := 10;

   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Initialize
     (Tile: out Tile_Type;
      ID  : in  Tile_ID_Type := Floor_Dirt);

   ------------------------
   --  Tile Subprograms  --
   ------------------------
   function Get_ID      (Tile : in Tile_Type) return Tile_ID_Type;
   function Get_Height  (Tile : in Tile_Type) return Integer;
   function Is_Walkable (Tile : in Tile_Type) return Boolean;

private
   type Tile_Type is tagged
      record
	 ID : Tile_ID_Type;
      end record;
end Tile;
