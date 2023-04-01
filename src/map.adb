with Unchecked_Deallocation;

package body Map is
   procedure Free is new Unchecked_Deallocation (Tile_Array, Tile_Array_Access);

   procedure Create (Map : in out Map_Type) is
      Size : C.int;
   begin
      Size := Map_Width'Last * Map_Height'Last;
      Map.Tiles := new Tile_Array (0 .. Size);
   end Create;

   procedure Render (Map : in out Map_Type; Video : in out Video_Driver) is
   begin
      null;
   end Render;

   procedure Finalize (Map : in out Map_Type) is
   begin
      if Map.Tiles /= null then
	 Free (Map.Tiles);
      end if;
   end Finalize;
end Map;
