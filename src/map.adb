with Unchecked_Deallocation;

package body Map is
   procedure Free is new Unchecked_Deallocation (Tile_Map, Tile_Map_Access);

   procedure Create (Map : in out Map_Type) is
   begin
      Map.Tiles := new Tile_Map;
 
      for Y in Map_Height_Type'Range loop
	 for X in Map_Width_Type'Range loop
	    Map.Tiles (Y)(X).Create (3);
	 end loop;
      end loop;
   end Create;

   procedure Render (Map : in out Map_Type; Video : in out Video_Driver) is
   begin
      for Y in Map_Height_Type'Range loop
	 for X in Map_Width_Type'Range loop
	    Video.Draw_Texture (X * 16, Y * 16, Map.Tiles (Y)(X).Get_ID);
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
