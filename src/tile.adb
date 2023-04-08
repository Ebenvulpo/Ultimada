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

   function Is_Walkable (Tile : in Tile_Type) return Boolean is
   begin
      case Tile.ID is
	 when
	   Floor_Lava  |
	   Floor_Lava2 |
	   Floor_Lava3 =>
	    return False;
	 when Rock_Pink => return False;
	 when Tree      => return False;
	 when Tree_Pink => return False;
	 when others    => return True;
      end case;
   end Is_Walkable;
end Tile;
