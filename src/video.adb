with Ada.Text_IO;
with Filepath;

package body Video is
   package C renames Interfaces.C;

   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize (Video : in out Video_Driver) is
   begin
      for I in Video.Object_Textures'Range loop
	 if Video.Object_Textures (I) /= null then
	    SDL_DestroyTexture (Video.Object_Textures (I));
	 end if;
      end loop;

      for I in Video.Person_Textures'Range loop
	 if Video.Person_Textures (I) /= null then
	    SDL_DestroyTexture (Video.Person_Textures (I));
	 end if;
      end loop;

      for I in Video.Tile_Textures'Range loop
	 if Video.Tile_Textures (I) /= null then
	    SDL_DestroyTexture (Video.Tile_Textures (I));
	 end if;
      end loop;

      if Video.Renderer /= null then
	 SDL_DestroyRenderer (Video.Renderer);
      end if;

      if Video.Window /= null then
	 SDL_DestroyWindow (Video.Window);
      end if;
   end Deinitialize;

   procedure Initialize (Video : in out Video_Driver) is
      Name : aliased C.char_array := "Ultimada" & C.nul;
   begin
      Ada.Text_IO.Put_Line ("Starting Video Driver");

      Video.Window := SDL_CreateWindow (Name'Address, 200, 200, 512, 512, 0);
      Video.Renderer := SDL_CreateRenderer (Video.Window, -1, 0);
      Video.Load_Textures;

      Ada.Text_IO.New_Line;
   end Initialize;

   -------------------------------
   --  Renderering Subprograms  --
   -------------------------------
   procedure Start (Video : in out Video_Driver) is
      Error : C.int;
   begin
      Error := SDL_SetRenderDrawColor (Video.Renderer, 16#00#, 16#00#, 16#00#, 16#FF#);
      if Error < 0 then
	 raise Program_Error;
      end if;

      SDL_RenderClear (Video.Renderer);
   end Start;

   procedure Finish (Video : in out Video_Driver) is
   begin
      SDL_RenderPresent (Video.Renderer);
   end Finish;

   procedure Draw_Rectangle
     (Video   : in out Video_Driver;
      W, H    : in     C.int;
      X, Y    : in     C.int;
      R, G, B : in     C.unsigned_char)
   is
      Error :         C.int;
      Rect  : aliased SDL_Rect;
   begin
      Rect.W := W;
      Rect.H := H;
      Rect.X := X;
      Rect.Y := Y;

      Error := SDL_SetRenderDrawColor (Video.Renderer, R, G, B, 16#FF#);
      if Error < 0 then
	 raise Program_Error;
      end if;

      Error := SDL_RenderFillRect (Video.Renderer, Rect'Access);
      if Error < 0 then
	 raise Program_Error;
      end if;
   end Draw_Rectangle;

   procedure Draw_Map_Tile
     (Video          : in out Video_Driver;
      X, Y           : in     C.int;
      Number         : in     Tile.Tile_ID_Type;
      Pixel_Offset_X : in     C.int;
      Pixel_Offset_Y : in     C.int)
   is
      Rect  : aliased SDL_Rect;
      Error :         C.int;
   begin
      Rect.X := (X * 16) + Pixel_Offset_X;
      Rect.Y := (Y * 16) + Pixel_Offset_Y;
      Rect.W := 16;
      Rect.H := 16;

      Error := SDL_RenderCopy (Video.Renderer, Video.Tile_Textures (Number), null, Rect'Access);
      if Error < 0 then
	 raise Program_Error;
      end if;
   end Draw_Map_Tile;

   procedure Draw_Object_Tile
     (Video          : in out Video_Driver;
      X, Y           : in     C.int;
      Number         : in     Object.Item_Type;
      Pixel_Offset_X : in     C.int;
      Pixel_Offset_Y : in     C.int)
   is
      Rect  : aliased SDL_Rect;
      Error :         C.int;
   begin
      Rect.X := (X * 16) + Pixel_Offset_X;
      Rect.Y := (Y * 16) + Pixel_Offset_Y;
      Rect.W := 16;
      Rect.H := 16;

      Error := SDL_RenderCopy (Video.Renderer, Video.Object_Textures (Number), null, Rect'Access);
      if Error < 0 then
	 raise Program_Error;
      end if;
   end Draw_Object_Tile;

   procedure Draw_Person_Tile
     (Video          : in out Video_Driver;
      X, Y           : in     C.int;
      Number         : in     Person.Person_Type;
      Pixel_Offset_X : in     C.int;
      Pixel_Offset_Y : in     C.int)
   is
      Rect  : aliased SDL_Rect;
      Error :         C.int;
   begin
      Rect.X := (X * 16) + Pixel_Offset_X;
      Rect.Y := (Y * 16) + Pixel_Offset_Y;
      Rect.W := 16;
      Rect.H := 16;

      Error := SDL_RenderCopy (Video.Renderer, Video.Person_Textures (Number), null, Rect'Access);
      if Error < 0 then
	 raise Program_Error;
      end if;
   end Draw_Person_Tile;

   procedure Change_Scale
     (Video   : in out Video_Driver;
      S       : in     C.int) is
      scale_error : C.int;
   begin
      scale_error := SDL_RenderSetLogicalSize(Video.Renderer,S,S);
      if scale_error = -1 then
         Ada.Text_IO.Put_Line ("Couldn't Scale");
      end if;
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
      Surface : SDL_Surface;
      Item    : Object.Item_Type;
   begin
      for I in Object_BMP_Array'Range loop
	 Ada.Text_IO.Put ("Loading: ");

	 declare
	    Name : constant String := SB.To_String (Object_BMP_Array (I).Name);
	 begin
	    Ada.Text_IO.Put_Line (Name);
	    Surface := SDL_LoadBMP (Filepath.Get (Name, "bmps"));
	 end;
	 SDL_SetColorKey (Surface, 1, 16#FF00CC#);

	 Item := Object_BMP_Array (I).Number;
	 --  Check if the texture is already used.
	 if Video.Object_Textures (Item) /= null then
	    raise Program_Error;
	 end if;
	 Video.Object_Textures (Item) := SDL_CreateTextureFromSurface (Video.Renderer, Surface);
	 SDL_FreeSurface (Surface);
      end loop;
   end Load_Objects;

   procedure Load_Persons (Video : in out Video_Driver) is
      Surface : SDL_Surface;
      P       : Person.Person_Type;
   begin
      for I in Person_BMP_Array'Range loop
	 Ada.Text_IO.Put ("Loading: ");

	 declare
	    Name : constant String := SB.To_String (Person_BMP_Array (I).Name);
	 begin
	    Ada.Text_IO.Put_Line (Name);
	    Surface := SDL_LoadBMP (Filepath.Get (Name, "bmps"));
	 end;
	 SDL_SetColorKey (Surface, 1, 16#FF00CC#);

	 P := Person_BMP_Array (I).Number;
	 --  Check if the texture is already used.
	 if Video.Person_Textures (P) /= null then
	    raise Program_Error;
	 end if;
	 Video.Person_Textures (P) := SDL_CreateTextureFromSurface (Video.Renderer, Surface);
	 SDL_FreeSurface (Surface);
      end loop;
   end Load_Persons;

   procedure Load_Tiles (Video : in out Video_Driver) is
      Surface : SDL_Surface;
      T       : Tile.Tile_ID_Type;
   begin
      for I in Tile_BMP_Array'Range loop
	 Ada.Text_IO.Put ("Loading: ");

	 declare
	    Name : constant String := SB.To_String (Tile_BMP_Array (I).Name);
	 begin
	    Ada.Text_IO.Put_Line (Name);
	   --   Surface := SDL_LoadBMP (Filepath.Get_BMP (Name));
	    Surface := SDL_LoadBMP (Filepath.Get (Name, "bmps"));
	 end;
	 SDL_SetColorKey (Surface, 1, 16#FF00CC#);

	 T := Tile_BMP_Array (I).Number;
	 --  Check if the texture is aleady used.
	 if Video.Tile_Textures (I) /= null then
	    raise Program_Error;
	 end if;
	 Video.Tile_Textures (T) := SDL_CreateTextureFromSurface (Video.Renderer, Surface);
	 SDL_FreeSurface (Surface);
      end loop;
   end Load_Tiles;
end Video;
