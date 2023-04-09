with Unchecked_Deallocation;

package body Object_Map is
   -------------------------------------
   --  Memory Management Subprograms  --
   -------------------------------------
   procedure Free is new Unchecked_Deallocation (Object_Map_Y, Object_Map_Access);

   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize (Object_Map : in out Object_Map_Type) is
   begin
      if Object_Map.Objects /= null then
	 Free (Object_Map.Objects);
      end if;
   end Deinitialize;

   procedure Initialize (Object_Map : in out Object_Map_Type) is
   begin
      if Object_Map.Objects /= null then
	 raise Program_Error;
      end if;

      Object_Map.Objects := new Object_Map_Y;

      for Y in Object_Map.Objects'Range loop
	 for X in Object_Map.Objects (Y)'Range loop
	    Object_Map.Objects (Y)(X).Initialize (Object.Item_None);
	 end loop;
      end loop;
   end Initialize;

   ------------------------------
   --  Object_Map Subprograms  --
   ------------------------------
   procedure Change_Object
     (Object_Map : in out Object_Map_Type;
      Item       : in     Object.Item_Type;
      X          : in     Object_Map_Width;
      Y          : in     Object_Map_Height)
   is
   begin
      Object_Map.Objects (Y)(X).Initialize (Item);
   end Change_Object;

   procedure Render
     (Object_Map     : in out Object_Map_Type;
      Video          : in out Video_Driver;
      Offset_X       : in     C.int;
      Offset_Y       : in     C.int;
      Pixel_Offset_X : in     C.int;
      Pixel_Offset_Y : in     C.int)
   is
   begin
      for Y in Object_Map_Height'Range loop
	 for X in Object_Map_Width'Range loop
	    if Object_Map.Objects (Y)(X).Get_Type /= Object.Item_None then
	       Video.Draw_Object_Tile (X + Offset_X, Y + Offset_Y, Object_Map.Objects (Y)(X).Get_Type, Pixel_Offset_X, Pixel_Offset_Y);
	    end if;
	 end loop;
      end loop;
   end Render;
end Object_Map;
