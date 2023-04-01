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

   procedure Draw_Texture
     (Video  : in out Video_Driver;
      X, Y   : in     C.int;
      Number : in     Natural)
   is
      Rect  : aliased SDL_Rect;
      Error :         C.int;
   begin
      Rect.X := X;
      Rect.Y := Y;
      Rect.W := 16;
      Rect.H := 16;

      Error := SDL_RenderCopy (Video.Renderer, Video.Textures (Number), null, Rect'Access);
      if Error < 0 then
	 raise Program_Error;
      end if;
   end Draw_Texture;

   procedure Init (Video : in out Video_Driver) is
      Name : aliased C.char_array := "Ultimada" & C.nul;
   begin
      Video.Window := SDL_CreateWindow (Name'Address, 200, 200, 640, 480, 0);
      Video.Renderer := SDL_CreateRenderer (Video.Window, -1, 0);

      Video.Load_Textures;
   end Init;

   procedure Finalize (Video : in out Video_Driver) is
   begin
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
      Surface := SDL_LoadBMP ("test.bmp");
      Video.Textures (0) := SDL_CreateTextureFromSurface (Video.Renderer, Surface);
      SDL_FreeSurface (Surface);
   end Load_Textures;
end Video;
