with Interfaces.C;
with System;       use System;

package SDL2 is
   package C renames Interfaces.C;

   type SDL_Window   is new System.Address;
   type SDL_Renderer is new System.Address;
   type SDL_Surface  is new System.Address;
   type SDL_Texture  is new System.Address;

   type Event_Padding is array (C.size_t range <>) of aliased C.unsigned_char;
   pragma Convention (C, Event_Padding);

   type SDL_Event is
      record
	Event_Type : C.int;
	Padding    : Event_Padding (1 .. 52);
      end record;
   pragma Convention (C, SDL_Event);

   type SDL_Rect is
      record
	 X, Y : aliased C.int;
	 W, H : aliased C.int;
      end record;
   pragma Convention (C, SDL_Rect);

   function SDL_WaitEvent (Event : access SDL_Event) return C.int with
     Import => True, Convention => C, External_Name => "SDL_WaitEvent";

   function SDL_Init (Flags : in C.unsigned) return C.int with
     Import => True, Convention => C, External_Name => "SDL_Init";

   procedure SDL_Quit with
     Import => True, Convention => C, External_Name => "SDL_Quit";

   function SDL_CreateWindow
     (Title : in C.char_array; X, Y, W, H : C.int; Flags : in C.unsigned)
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

   procedure SDL_RenderPresent (Renderer : in SDL_Renderer) with
     Import => True, Convention => C, External_Name => "SDL_RenderPresent";

   function SDL_CreateTextureFromSurface
     (Renderer : in SDL_Renderer; Surface : in SDL_Surface)
     return SDL_Texture with
     Import => True, Convention => C, External_Name => "SDL_CreateTextureFromSurface";

   procedure SDL_FreeSurface (Surface : in SDL_Surface)
     with Import => True, Convention => C, External_Name => "SDL_FreeSurface";

   function SDL_LoadBMP (File : in C.char_array) return SDL_Surface;
end SDL2;
