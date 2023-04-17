with SDL2_Stdinc; use SDL2_Stdinc;

package SDL2_Surface is
   type Dummy_SDL_Surface is limited private;
   type SDL_Surface       is access Dummy_SDL_Surface;
   pragma Convention (C, SDL_Surface);

   --------------------------
   --  Public Subprograms  --
   --------------------------
   procedure Free_Surface (Surface : in out SDL_Surface);

   function Load_BMP (File : in String) return SDL_Surface;

   procedure Set_Color_Key
     (Surface : in SDL_Surface;
      Flag    : in Boolean;
      Key     : in Uint32);

   ------------------
   --  Exceptions  --
   ------------------
   Surface_Error : exception;

private
   type Dummy_SDL_Surface is null record;
   pragma Convention (C, Dummy_SDL_Surface);
end SDL2_Surface;
