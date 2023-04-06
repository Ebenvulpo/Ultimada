with Rand;                   use Rand;
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
      low_tile     : in     Integer;
      high_tile    : in     Integer;
      default_tile : in     Integer;
      spawn_rate   : in     Float)
   is
      R : Integer;
      F : Float;
   begin
      Map.Tiles := new Tile_Map;

      for Y in Map_Height_Type'Range loop
         for X in Map_Width_Type'Range loop
            F := randomF;

            R := default_tile;
            if F < spawn_rate then
               R := randomN(low_tile, high_tile);
            end if;

            Map.Tiles (Y)(X).Initialize (R);
         end loop;
      end loop;
   end Initialize;

   -----------------------
   --  Map Subprograms  --
   -----------------------
   procedure Render
     (Map            : in out Map_Type;
      Video          : in out Video_Driver;
      Offset_X       : in     C.int;
      Offset_Y       : in     C.int;
      Pixel_Offset_X : in     C.int;
      Pixel_Offset_Y : in     C.int)
   is
   begin
      for Y in Map_Height_Type'Range loop
         for X in Map_Width_Type'Range loop
            Video.Draw_Map_Tile (X + Offset_X, Y + Offset_Y, Map.Tiles (Y)(X).Get_ID, Pixel_Offset_X, Pixel_Offset_Y);
         end loop;
      end loop;
   end Render;
end Map;
