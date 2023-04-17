with Rand;
with Unchecked_Deallocation;

package body Map is
   -------------------------------------
   --  Memory Management Subprograms  --
   -------------------------------------
   procedure Free is new Unchecked_Deallocation (Tile_Map, Tile_Map_Access);

   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize (Map : in out Map_Type) is
   begin
      if Map.Tiles /= null then
	 Free (Map.Tiles);
      end if;
   end Deinitialize;

   procedure Initialize
     (Map          : in out Map_Type;
      Low_Tile     : in     Integer;
      High_Tile    : in     Integer;
      Default_Tile : in     Integer;
      Spawn_Rate   : in     Float)
   is
   begin
      Map.Tiles := new Tile_Map;

      Map.Generate_Grass;
      Map.Generate_Poles;
      Map.Generate_Lava_Pools;
      Map.Generate_Forests;
   end Initialize;

   -----------------------
   --  Map Subprograms  --
   -----------------------
   procedure Generate_Grass (Map : in out Map_Type) is
      G : Integer;
      T : Tile.Tile_ID_Type;
   begin
      for Y in Map.Tiles'Range loop
	 for X in Map.Tiles (Y)'Range loop
	    G := Rand.RandomN (0, 2);
	    case G is
	       when 0 => T := Tile.Floor_Grass;
	       when 1 => T := Tile.Floor_Grass2;
	       when 2 => T := Tile.Floor_Grass3;
	       when others => raise Program_Error;
	    end case;

	    Map.Tiles (Y)(X).Initialize (T);
	 end loop;
      end loop;
   end Generate_Grass;

   procedure Generate_Forests (Map : in out Map_Type) is
   begin
      for I in Natural range 0 .. 1_000 loop
	 Map.Trees;
      end loop;
   end Generate_Forests;

   procedure Generate_Lava (Map : in out Map_Type) is
      Location_X : Map_Width_Type;
      Location_Y : Map_Height_Type;
      L : Integer;
      T : Tile.Tile_ID_Type;
   begin
      Location_X := Map_Width_Type  (Rand.RandomN (0, Integer (Map_Width_Type'Last)  - 3));
      Location_Y := Map_Height_Type (Rand.RandomN (0, Integer (Map_Height_Type'Last) - 3));

      for Y in Map_Height_Type range Location_Y .. Location_Y + 3 loop
	 for X in Map_Width_Type range Location_X .. Location_X + 3 loop
	    L := Rand.RandomN (0, 2);
	    case L is
	       when 0 => T := Tile.Floor_Lava;
	       when 1 => T := Tile.Floor_Lava2;
	       when 2 => T := Tile.Floor_Lava3;
	       when others => raise Program_Error;
	    end case;

	    if Rand.RandomF > 0.15 then
	       Map.Tiles (Y)(X).Initialize (T);
	    end if;
	 end loop;
      end loop;
   end Generate_Lava;

   procedure Generate_Lava_Pools (Map : in out Map_Type) is
   begin
      for I in Natural range 0 .. 64 loop
	 Map.Generate_Lava;
      end loop;
   end Generate_Lava_Pools;

   procedure Generate_Poles (Map : in out Map_Type) is
      Y_F : Map_Height_Type;
      Y_L : Map_Height_Type;
      F   : Float;
   begin
      Y_F := Map_Height_Type'Last / 8;
      Y_L := Map_Height_Type'Last - Y_F;

      --  North Pole
      for Y in Map_Height_Type range 0 .. Y_F loop
	 for X in Map.Tiles (Y)'Range loop
	    F := 1.0 - (Float (Y) / Float (Y_F));

	    if Rand.RandomF < F then
	       Map.Tiles (Y)(X).Initialize (Tile.Floor_Ice);
	    end if;
	 end loop;
      end loop;

      --  South Pole
      for Y in Map_Height_Type range Y_L .. Map_Height_Type'Last loop
	 for X in Map.Tiles (Y)'Range loop
	    F := 1.0 - (Float (Map_Height_Type'Last - Y) / Float (Y_F));

	    if Rand.RandomF < F then
	       Map.Tiles (Y)(X).Initialize (Tile.Floor_Ice);
	    end if;
	 end loop;
      end loop;
   end Generate_Poles;

   procedure Trees (Map : in out Map_Type) is
      Location_X : Map_Width_Type;
      Location_Y : Map_Height_Type;
   begin
      Location_X := Map_Width_Type  (Rand.RandomN (0, Integer (Map_Width_Type'Last)  - 3));
      Location_Y := Map_Height_Type (Rand.RandomN (0, Integer (Map_Height_Type'Last) - 3));

      for Y in Map_Height_Type range Location_Y .. Location_Y + 3 loop
	 for X in Map_Width_Type range Location_X .. Location_X + 3 loop
	    if
	      Map.Tiles (Y)(X).Get_Height > 0 and
	      Rand.RandomF > 0.25
	    then
	       Map.Tiles (Y)(X).Initialize (Tile.Tree);
	    end if;
	 end loop;
      end loop;
   end Trees;

   function Get_Tile
     (Map : in out Map_Type;
      X   : in     Map_Width_Type;
      Y   : in     Map_Height_Type)
     return Tile_Type
   is
   begin
      return Map.Tiles (Y)(X);
   end Get_Tile;

   function Is_Tile_Walkable
     (Map : in out Map_Type;
      X   : in     Map_Width_Type'Base;
      Y   : in     Map_Height_Type'Base)
     return Boolean
   is
   begin
      if X < 0 or X > Map_Width_Type'Last then
	 return False;
      end if;

      if Y < 0 or Y > Map_Height_Type'Last then
	 return False;
      end if;

      return Map.Get_Tile (X, Y).Is_Walkable;
   end Is_Tile_Walkable;

   procedure Render
     (Map            : in out Map_Type;
      Video          : in out Video_Driver;
      Offset_X       : in     Sint32;
      Offset_Y       : in     Sint32;
      Pixel_Offset_X : in     Sint32;
      Pixel_Offset_Y : in     sint32)
   is
   begin
      for Y in Map_Height_Type'Range loop
         for X in Map_Width_Type'Range loop
            Video.Draw_Map_Tile (X + Offset_X, Y + Offset_Y, Map.Tiles (Y)(X).Get_ID, Pixel_Offset_X, Pixel_Offset_Y);
         end loop;
      end loop;
   end Render;
end Map;
