with SDL2_Stdinc; use SDL2_Stdinc;

package SDL2_Video is
   type Dummy_SDL_Window is limited private;
   type SDL_Window       is access Dummy_SDL_Window;
   pragma Convention (C, SDL_Window);

   subtype Window_Width_Type  is Sint32 range 1 .. 2**15 - 1;
   subtype Window_Height_Type is Sint32 range 1 .. 2**15 - 1;

   --------------------------
   --  Public Subprograms  --
   --------------------------
   function Create_Window
     (Title  : in String;
      Width  : in Window_Width_Type;
      Height : in Window_Height_Type)
     return SDL_Window;

   procedure Destroy_Window (Window : in out SDL_Window);

   -----------------
   -- Exceptions  --
   -----------------
   Window_Error : exception;

private
   type Dummy_SDL_Window is null record;
   pragma Convention (C, Dummy_SDL_Window);
end SDL2_Video;
