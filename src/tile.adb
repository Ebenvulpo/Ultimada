package body Tile is
   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Initialize (Tile : out Tile_Type; ID : in Tile_ID_Type := 0) is
   begin
      Tile.ID := ID;
   end Initialize;

   ------------------------
   --  Tile Subprograms  --
   ------------------------
   function Get_ID (Tile : in Tile_Type) return Tile_ID_Type is
   begin
      return Tile.ID;
   end Get_ID;
end Tile;
