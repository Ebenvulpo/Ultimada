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

   procedure Init (Video : in out Video_Driver) is
      Name : aliased C.char_array := "Ultimada" & C.nul;
   begin
      Video.Window := SDL_CreateWindow (Name'Address, 200, 200, 640, 480, 0);
      Video.Renderer := SDL_CreateRenderer (Video.Window, -1, 0);
   end Init;

   procedure Finalize (Object : in out Video_Driver) is
   begin
      if Object.Renderer /= null then
	 SDL_DestroyRenderer (Object.Renderer);
      end if;

      if Object.Window /= null then
	 SDL_DestroyWindow (Object.Window);
      end if;
   end Finalize;
end Video;
