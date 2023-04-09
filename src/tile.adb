package body Tile is
   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Initialize
     (Tile : out Tile_Type;
      ID   : in  Tile_ID_Type := Floor_Dirt)
   is
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

   function Get_Height (Tile : in Tile_Type) return Integer is
   begin
      case Tile.ID is
	 when Rock_Pink => return 2;
	 when
	   Floor_Dirt   |
	   Floor_Grass  |
	   Floor_Grass2 |
	   Floor_Grass3 => return 1;
	 when
	   Floor_Ice   |
	   Floor_Lava  |
	   Floor_Lava2 |
	   Floor_Lava3 => return 0;
	 when others   => return 0;
      end case;
   end Get_Height;

   function Is_Walkable (Tile : in Tile_Type) return Boolean is
   begin
      case Tile.ID is
	 when
	   Floor_Lava  |
	   Floor_Lava2 |
	   Floor_Lava3 =>
	    return False;
	 when Floor_Ice => return True;
	 when Rock_Pink => return False;
	 when Tree      => return False;
	 when Tree_Pink => return False;
	 when others    => return True;
      end case;
   end Is_Walkable;
end Tile;
