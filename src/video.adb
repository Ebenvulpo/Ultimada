with Ada.Text_IO;
with Filepath;
with SDL2_Rect;
with SDL2_Surface;

package body Video is
   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize (Video : in out Video_Driver) is
   begin
      for I in Video.Object_Textures'Range loop
	 if Video.Object_Textures (I) /= null then
	    SDL2_Render.Destroy_Texture (Video.Object_Textures (I));
	 end if;
      end loop;

      for I in Video.Person_Textures'Range loop
	 if Video.Person_Textures (I) /= null then
	    SDL2_Render.Destroy_Texture (Video.Person_Textures (I));
	 end if;
      end loop;

      for I in Video.Tile_Textures'Range loop
	 if Video.Tile_Textures (I) /= null then
	    SDL2_Render.Destroy_Texture (Video.Tile_Textures (I));
	 end if;
      end loop;

      if Video.Renderer /= null then
	 SDL2_Render.Destroy_Renderer (Video.Renderer);
      end if;

      if Video.Window /= null then
	 SDL2_Video.Destroy_Window (Video.Window);
      end if;
   end Deinitialize;

   procedure Initialize (Video : in out Video_Driver) is
   begin
      Ada.Text_IO.Put_Line ("Starting Video Driver");

      Video.Window := SDL2_Video.Create_Window ("Ultimada", 512, 512);
      Video.Renderer := SDL2_Render.Create_Renderer (Video.Window);
      Video.Load_Textures;

      Ada.Text_IO.New_Line;
   end Initialize;

   -------------------------------
   --  Renderering Subprograms  --
   -------------------------------
   procedure Start (Video : in out Video_Driver) is
   begin
      SDL2_Render.Set_Render_Draw_Color (Video.Renderer, 16#00#, 16#00#, 16#00#, 16#FF#);
      SDL2_Render.Render_Clear (Video.Renderer);
   end Start;

   procedure Finish (Video : in out Video_Driver) is
   begin
      SDL2_Render.Render_Present (Video.Renderer);
   end Finish;

   procedure Draw_Rectangle
     (Video   : in out Video_Driver;
      W, H    : in     Sint32;
      X, Y    : in     Sint32;
      R       : in     Red_Color_Channel_Type;
      G       : in     Green_Color_Channel_Type;
      B       : in     Blue_Color_Channel_Type)
   is
      Rect : aliased SDL2_Rect.SDL_Rectangle;
   begin
      Rect.W := W;
      Rect.H := H;
      Rect.X := X;
      Rect.Y := Y;

      SDL2_Render.Set_Render_Draw_Color (Video.Renderer, R, G, B, 16#FF#);
      SDL2_Render.Render_Fill_Rectangle (Video.Renderer, Rect'Access);
   end Draw_Rectangle;

   procedure Draw_Map_Tile
     (Video          : in out Video_Driver;
      X, Y           : in     Sint32;
      Number         : in     Tile.Tile_ID_Type;
      Pixel_Offset_X : in     Sint32;
      Pixel_Offset_Y : in     Sint32)
   is
      Rect : aliased SDL2_Rect.SDL_Rectangle;
   begin
      Rect.X := (X * 16) + Pixel_Offset_X;
      Rect.Y := (Y * 16) + Pixel_Offset_Y;
      Rect.W := 16;
      Rect.H := 16;

      SDL2_Render.Render_Copy (Video.Renderer, Video.Tile_Textures (Number), null, Rect'Access);
   end Draw_Map_Tile;

   procedure Draw_Object_Tile
     (Video          : in out Video_Driver;
      X, Y           : in     Sint32;
      Number         : in     Object.Item_Type;
      Pixel_Offset_X : in     Sint32;
      Pixel_Offset_Y : in     Sint32)
   is
      Rect : aliased SDL2_Rect.SDL_Rectangle;
   begin
      Rect.X := (X * 16) + Pixel_Offset_X;
      Rect.Y := (Y * 16) + Pixel_Offset_Y;
      Rect.W := 16;
      Rect.H := 16;

      SDL2_Render.Render_Copy (Video.Renderer, Video.Object_Textures (Number), null, Rect'Access);
   end Draw_Object_Tile;

   procedure Draw_Person_Tile
     (Video          : in out Video_Driver;
      X, Y           : in     Sint32;
      Number         : in     Person.Person_Type;
      Pixel_Offset_X : in     Sint32;
      Pixel_Offset_Y : in     Sint32)
   is
      Rect : aliased SDL2_Rect.SDL_Rectangle;
   begin
      if Number = Person.Person_None then
	 return;
      end if;

      Rect.X := (X * 16) + Pixel_Offset_X;
      Rect.Y := (Y * 16) + Pixel_Offset_Y;
      Rect.W := 16;
      Rect.H := 16;

      SDL2_Render.Render_Copy (Video.Renderer, Video.Person_Textures (Number), null, Rect'Access);
   end Draw_Person_Tile;

   procedure Change_Scale
     (Video   : in out Video_Driver;
      S       : in     Integer)
   is
   begin
      SDL2_Render.Render_Set_Logical_Size(Video.Renderer, S, S);
   end Change_Scale;

   ---------------------------
   --  Private Subprograms  --
   ---------------------------
   procedure Load_Textures (Video : in out Video_Driver) is
   begin
      Ada.Text_IO.Put_Line ("Loading Textures...");
      Load_Objects (Video);
      Load_Persons (Video);
      Load_Tiles   (Video);
   end Load_Textures;

   procedure Load_Objects (Video : in out Video_Driver) is
      Surface : SDL2_Surface.SDL_Surface;
      Item    : Object.Item_Type;
   begin
      for I in Object_BMP_Array'Range loop
	 Ada.Text_IO.Put ("Loading: ");

	 declare
	    Name : constant String := SB.To_String (Object_BMP_Array (I).Name);
	 begin
	    Ada.Text_IO.Put_Line (Name);
	    Surface := SDL2_Surface.Load_BMP (Filepath.Get (Name, "bmps"));
	 end;
	 SDL2_Surface.Set_Color_Key (Surface, True, 16#FF00CC#);

	 Item := Object_BMP_Array (I).Number;
	 --  Check if the texture is already used.
	 if Video.Object_Textures (Item) /= null then
	    raise Program_Error;
	 end if;
	 Video.Object_Textures (Item) := SDL2_Render.Create_Texture_From_Surface (Video.Renderer, Surface);
	 SDL2_Surface.Free_Surface (Surface);
      end loop;
   end Load_Objects;

   procedure Load_Persons (Video : in out Video_Driver) is
      Surface : SDL2_Surface.SDL_Surface;
      P       : Person.Person_Type;
   begin
      for I in Person_BMP_Array'Range loop
	 Ada.Text_IO.Put ("Loading: ");

	 declare
	    Name : constant String := SB.To_String (Person_BMP_Array (I).Name);
	 begin
	    Ada.Text_IO.Put_Line (Name);
	    Surface := SDL2_Surface.Load_BMP (Filepath.Get (Name, "bmps"));
	 end;
	 SDL2_Surface.Set_Color_Key (Surface, True, 16#FF00CC#);

	 P := Person_BMP_Array (I).Number;
	 --  Check if the texture is already used.
	 if Video.Person_Textures (P) /= null then
	    raise Program_Error;
	 end if;
	 Video.Person_Textures (P) := SDL2_Render.Create_Texture_From_Surface (Video.Renderer, Surface);
	 SDL2_Surface.Free_Surface (Surface);
      end loop;
   end Load_Persons;

   procedure Load_Tiles (Video : in out Video_Driver) is
      Surface : SDL2_Surface.SDL_Surface;
      T       : Tile.Tile_ID_Type;
   begin
      for I in Tile_BMP_Array'Range loop
	 Ada.Text_IO.Put ("Loading: ");

	 declare
	    Name : constant String := SB.To_String (Tile_BMP_Array (I).Name);
	 begin
	    Ada.Text_IO.Put_Line (Name);
	   --   Surface := SDL_LoadBMP (Filepath.Get_BMP (Name));
	    Surface := SDL2_Surface.Load_BMP (Filepath.Get (Name, "bmps"));
	 end;
	 SDL2_Surface.Set_Color_Key (Surface, True, 16#FF00CC#);

	 T := Tile_BMP_Array (I).Number;
	 --  Check if the texture is aleady used.
	 if Video.Tile_Textures (I) /= null then
	    raise Program_Error;
	 end if;
	 Video.Tile_Textures (T) := SDL2_Render.Create_Texture_From_Surface (Video.Renderer, Surface);
	 SDL2_Surface.Free_Surface (Surface);
      end loop;
   end Load_Tiles;
end Video;
