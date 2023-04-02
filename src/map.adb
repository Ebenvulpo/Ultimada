with Unchecked_Deallocation;
with Ada.Text_IO;
with Rand;       use Rand;

package body Map is
   procedure Free is new Unchecked_Deallocation (Tile_Map, Tile_Map_Access);

   procedure Create
     (Map          : in out Map_Type;
      low_tile     : in Integer;
      high_tile    : in Integer;
      default_tile : in Integer;
      spawn_rate   : in Float)
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
               --  Ada.Text_IO.Put_Line(low_tile'Image & " "
               --     & high_tile'Image & " "
               --     & F'Image & " "
               --     & R'Image & " "
               --     );
               R := randomN(low_tile, high_tile);
            end if;

            Map.Tiles (Y)(X).Create ( R );
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
