with Interfaces.C; use Interfaces.C;
with System;       use System;

package SDL2 is
   package C renames Interfaces.C;

   type Dummy_SDL_Window   is limited private;
   type Dummy_SDL_Renderer is limited private;
   type Dummy_SDL_Surface  is limited private;
   type Dummy_SDL_Texture  is limited private;

   type SDL_Window   is access Dummy_SDL_Window;
   type SDL_Renderer is access Dummy_SDL_Renderer;
   type SDL_Surface  is access Dummy_SDL_Surface;
   type SDL_Texture  is access Dummy_SDL_Texture;

   pragma Convention (C, SDL_Window);
   pragma Convention (C, SDL_Renderer);
   pragma Convention (C, SDL_Surface);
   pragma Convention (C, SDL_Texture);

   type Event_Padding is array (C.size_t range <>) of aliased C.unsigned_char;
   pragma Convention (C, Event_Padding);

   type SDL_Event is
      record
	Event_Type : C.unsigned;
	Padding    : Event_Padding (1 .. 52);
      end record;
   pragma Convention (C, SDL_Event);

   type SDL_Rect is
      record
	 X, Y : aliased C.int;
	 W, H : aliased C.int;
      end record;
   pragma Convention (C, SDL_Rect);

   procedure SDL_WaitEvent (Event : access SDL_Event) with
     Import => True, Convention => C, External_Name => "SDL_WaitEvent";

   function SDL_Init (Flags : in C.unsigned) return C.int with
     Import => True, Convention => C, External_Name => "SDL_Init";

   procedure SDL_Quit with
     Import => True, Convention => C, External_Name => "SDL_Quit";

   function SDL_CreateWindow
     (Title : in System.Address; X, Y, W, H : C.int; Flags : in C.unsigned)
     return SDL_Window with
     Import => True, Convention => C, External_Name => "SDL_CreateWindow";

   procedure SDL_DestroyWindow (Window : in SDL_Window) with
     Import => True, Convention => C, External_Name => "SDL_DestroyWindow";

   function SDL_CreateRenderer
     (Window : in SDL_Window; Index : C.int; Flags : in C.unsigned)
     return SDL_Renderer with
     Import => True, Convention => C, External_Name => "SDL_CreateRenderer";

   procedure SDL_DestroyRenderer (Renderer : in SDL_Renderer) with
     Import => True, Convention => C, External_Name => "SDL_DestroyRenderer";

   function SDL_SetRenderDrawColor
     (Renderer : in SDL_Renderer; R, G, B, A : in C.unsigned_char)
     return C.int with
     Import => True, Convention => C, External_Name => "SDL_SetRenderDrawColor";

   function SDL_RenderCopy
     (Renderer : in     SDL_Renderer;
      Texture  : in     SDL_Texture;
      SrcRect  : access SDL_Rect;
      DstRect  : access SDL_Rect)
     return C.int with
     Import => True, Convention => C, External_Name => "SDL_RenderCopy";

   procedure SDL_RenderClear (Renderer : in SDL_Renderer) with
     Import => True, Convention => C, External_Name => "SDL_RenderClear";

   function SDL_RenderFillRect
     (Renderer : in SDL_Renderer; Rect : access SDL_Rect)
     return C.int with
     Import => True, Convention => C, External_Name => "SDL_RenderFillRect";

   procedure SDL_RenderPresent (Renderer : in SDL_Renderer) with
     Import => True, Convention => C, External_Name => "SDL_RenderPresent";

   function SDL_CreateTextureFromSurface
     (Renderer : in SDL_Renderer; Surface : in SDL_Surface)
     return SDL_Texture with
     Import => True, Convention => C, External_Name => "SDL_CreateTextureFromSurface";

   procedure SDL_FreeSurface (Surface : in SDL_Surface) with
     Import => True, Convention => C, External_Name => "SDL_FreeSurface";

   function SDL_LoadBMP (File : in String) return SDL_Surface;

private
   type Dummy_SDL_Window   is null record;
   type Dummy_SDL_Renderer is null record;
   type Dummy_SDL_Surface  is null record;
   type Dummy_SDL_Texture  is null record;
end SDL2;
