with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings;
with System;               use System;
with Unchecked_Conversion;

package SDL2 is
   package C renames Interfaces.C;

   type Dummy_Mix_Chunk    is limited private;
   type Dummy_Mix_Music    is limited private;
   type Dummy_SDL_Window   is limited private;
   type Dummy_SDL_Renderer is limited private;
   type Dummy_SDL_Surface  is limited private;
   type Dummy_SDL_Texture  is limited private;

   type Mix_Chunk    is access Dummy_Mix_Chunk;
   type Mix_Music    is access Dummy_Mix_Music;
   type SDL_Window   is access Dummy_SDL_Window;
   type SDL_Renderer is access Dummy_SDL_Renderer;
   type SDL_Surface  is access Dummy_SDL_Surface;
   type SDL_Texture  is access Dummy_SDL_Texture;

   pragma Convention (C, Mix_Chunk);
   pragma Convention (C, Mix_Music);
   pragma Convention (C, SDL_Window);
   pragma Convention (C, SDL_Renderer);
   pragma Convention (C, SDL_Surface);
   pragma Convention (C, SDL_Texture);

   type SDL_Keysym is
      record
	 Scancode     : aliased C.int;
	 Sym          : aliased C.int;
	 Modification : aliased C.unsigned_short;
	 Unused       : aliased C.unsigned;
      end record;
   pragma Convention (C, SDL_Keysym);

   type SDL_KeyboardEvent is
      record
	 Event_Type : aliased C.unsigned;
	 Timestamp  : aliased C.unsigned;
	 WindowID   : aliased C.unsigned;
	 State      : aliased C.unsigned_char;
	 Repeat     : aliased C.unsigned_char;
	 Padding2   : aliased C.unsigned_char;
	 Padding3   : aliased C.unsigned_char;
	 Keysym     : aliased SDL_Keysym;
      end record;
   for SDL_KeyboardEvent'Size use 56 * 8;
   pragma Convention (C, SDL_KeyboardEvent);

   type SDL_Event is
      record
	Event_Type : C.unsigned;
      end record;
   for SDL_Event'Size use 56 * 8;
   pragma Convention (C, SDL_Event);

   type SDL_Rect is
      record
	 X, Y : aliased C.int;
	 W, H : aliased C.int;
      end record;
   pragma Convention (C, SDL_Rect);

   function Get_KeyboardEvent is new Unchecked_Conversion (SDL_Event, SDL_KeyboardEvent);

   function SDL_GetBasePath return C.Strings.chars_ptr with
     Import => True, Convention => C, External_Name => "SDL_GetBasePath";

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

   function SDL_RenderSetLogicalSize
     (Renderer : in SDL_Renderer; W, H : C.int)
     return C.int with
     Import => True, Convention => C, External_Name => "SDL_RenderSetLogicalSize";

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

   procedure SDL_SetColorKey
     (Surface : in SDL_Surface;
      Flag    : in C.int;
      Key     : in C.unsigned) with
     Import => True, Convention => C, External_Name => "SDL_SetColorKey";

   procedure SDL_DestroyTexture (Texture : in SDL_Texture) with
     Import => True, Convention => C, External_Name => "SDL_DestroyTexture";

   procedure SDL_FreeSurface (Surface : in SDL_Surface) with
     Import => True, Convention => C, External_Name => "SDL_FreeSurface";

   function SDL_LoadBMP (File : in String) return SDL_Surface;

   function Mix_Init (Flags : in C.int) return C.int with
     Import => True, Convention => C, External_Name => "Mix_Init";

   procedure Mix_Quit with
     Import => True, Convention => C, External_Name => "Mix_Quit";

   function Mix_OpenAudio
     (Frequencey : in C.int;
      Format     : in C.unsigned_short;
      Channel    : in C.int;
      Chunksize  : in C.int)
     return C.int with
     Import => True, Convention => C, External_Name => "Mix_OpenAudio";

   function Mix_LoadWAV (File : in String) return Mix_Chunk;

   function Mix_PlayChannel
     (Channel : in C.int;
      Chunk   : in Mix_Chunk;
      Loops   : in C.int)
     return C.int with
     Import => True, Convention => C, External_Name => "Mix_PlayChannel";

   procedure Mix_FreeChunk (Chunk : in Mix_Chunk) with
     Import => True, Convention => C, External_Name => "Mix_FreeChunk";

   function Mix_LoadMUS (File : in C.Strings.chars_ptr) return Mix_Music with
     Import => True, Convention => C, External_Name => "Mix_LoadMUS";

   procedure Mix_FreeMusic (Music : in Mix_Music) with
     Import => True, Convention => C, External_Name => "Mix_FreeMusic";

private
   type Dummy_Mix_Chunk    is null record;
   type Dummy_Mix_Music    is null record;
   type Dummy_SDL_Window   is null record;
   type Dummy_SDL_Renderer is null record;
   type Dummy_SDL_Surface  is null record;
   type Dummy_SDL_Texture  is null record;
end SDL2;
