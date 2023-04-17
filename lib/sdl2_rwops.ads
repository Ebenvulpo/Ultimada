package SDL2_RWops is
   type Dummy_SDL_RWops is limited private;
   type SDL_RWops       is access Dummy_SDL_RWops;
   pragma Convention (C, SDL_RWops);

   --------------------------
   --  Public Subprograms  --
   --------------------------
   function RW_From_File
     (File : in String;
      Mode : in String)
     return SDL_RWops;

   ------------------
   --  Exceptions  --
   ------------------
   RWops_Error : exception;
private
   type Dummy_SDL_RWops is null record;
   pragma Convention (C, Dummy_SDL_RWops);
end SDL2_RWops;
