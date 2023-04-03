with Ada.Text_IO;
with Filepath;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Video is
   package C renames Interfaces.C;

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

   procedure Draw_Tile
     (Video  : in out Video_Driver;
      X, Y   : in     C.int;
      Number : in     Natural)
   is
      Rect  : aliased SDL_Rect;
      Error :         C.int;
   begin
      Rect.X := X * 16;
      Rect.Y := Y * 16;
      Rect.W := 16;
      Rect.H := 16;

      Error := SDL_RenderCopy (Video.Renderer, Video.Textures (Number), null, Rect'Access);
      if Error < 0 then
	 raise Program_Error;
      end if;
   end Draw_Tile;

   procedure Init (Video : in out Video_Driver) is
      Name : aliased C.char_array := "Ultimada" & C.nul;
   begin
      Video.Window := SDL_CreateWindow (Name'Address, 200, 200, 512, 512, 0);
      Video.Renderer := SDL_CreateRenderer (Video.Window, -1, 0);
      Video.Load_Textures;
   end Init;

   procedure Change_scale
     (Video   : in out Video_Driver;
      S       : in     C.int) is
      scale_error : C.int;
   begin
      scale_error := SDL_RenderSetLogicalSize(Video.Renderer,S,S);
      if scale_error = -1 then
         Ada.Text_IO.Put_Line("Couldn't Scale");
      end if;
   end Change_scale;


   procedure Finalize (Video : in out Video_Driver) is
   begin
      for I in Video.Textures'Range loop
	 if Video.Textures (I) /= null then
	    SDL_DestroyTexture (Video.Textures (I));
	 end if;
      end loop;

      if Video.Renderer /= null then
	 SDL_DestroyRenderer (Video.Renderer);
      end if;

      if Video.Window /= null then
	 SDL_DestroyWindow (Video.Window);
      end if;
   end Finalize;

   procedure Load_Textures (Video : in out Video_Driver) is
      Surface : SDL_Surface;
   begin
      for I in Bitmap_Array'Range loop
	 Ada.Text_IO.Put      ("Loading: ");
	 Ada.Text_IO.Put_Line (SB.To_String (Bitmap_Array (I)));

	 Surface := SDL_LoadBMP (Value (Filepath.Get) & "assets/" & "bmps/" & SB.To_String (Bitmap_Array (I)));
	 SDL_SetColorKey (Surface, 1, 16#FF00CC#);
	 Video.Textures (I) := SDL_CreateTextureFromSurface (Video.Renderer, Surface);
	 SDL_FreeSurface (Surface);
      end loop;
   end Load_Textures;
end Video;
