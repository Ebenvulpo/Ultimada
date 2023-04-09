package body Object is
   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Initialize
     (Object : in out Object_Type;
      Item   : in     Item_Type)
   is
   begin
      Object.Item := Item;
   end Initialize;

   --------------------------
   --  Object Subprograms  --
   --------------------------
   function Get_Type (Object : in Object_Type) return Item_Type is
   begin
      return Object.Item;
   end Get_Type;
   
   function Is_Placeable (T : in Tile.Tile_ID_Type) return Boolean is
   begin
      case T is
	 when
	   Tile.Floor_Lava  |
	   Tile.Floor_Lava2 |
	   Tile.Floor_Lava3 => return False;
	 when
	   Tile.Tree |
	   Tile.Tree_Pink    => return False;
	 when Tile.Rock_Pink => return False;
	 when others => return True;
      end case;
   end Is_Placeable;
end Object;
