with Unchecked_Deallocation;
with Tile;             use Tile;

package body Map is
   procedure Free is new Unchecked_Deallocation (Tile_Map, Tile_Map_Access);

   procedure Create (Map : in out Map_Type; tile : in Tile_ID_Type) is
   begin
      Map.Tiles := new Tile_Map;

      for Y in Map_Height_Type'Range loop
         for X in Map_Width_Type'Range loop
            Map.Tiles (Y)(X).Create ( tile );
         end loop;
      end loop;
   end Create;

   procedure Render
     (Map      : in out Map_Type;
      Video    : in out Video_Driver;
      Offset_X : in     C.int;
      Offset_Y : in     C.int)
   is
   begin
      for Y in Map_Height_Type'Range loop
         for X in Map_Width_Type'Range loop
            Video.Draw_Tile ((X + Offset_X), (Y + Offset_Y), Map.Tiles (Y)(X).Get_ID);
         end loop;
      end loop;
   end Render;

   procedure Finalize (Map : in out Map_Type) is
   begin
      if Map.Tiles /= null then
         Free (Map.Tiles);
      end if;
   end Finalize;
end Map;
