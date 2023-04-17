with Interfaces.C;
with SDL2_RWops;   use SDL2_RWops;

package body SDL2_Surface is
   package C renames Interfaces.C;

   ----------------------------
   --  External C Functions  --
   ----------------------------
   procedure C_SDL_FreeSurface (Surface : in SDL_Surface) with
     Import => True, Convention => C, External_Name => "SDL_FreeSurface";

   function C_SDL_LoadBMP_RW
     (File    : in SDL_RWops;
      FreeSrc : in C.int)
     return SDL_Surface with
     Import => True, Convention => C, External_Name => "SDL_LoadBMP_RW";

   procedure C_SDL_SetColorKey
     (Surface : in SDL_Surface;
      Flags   : in C.int;
      Key     : in Uint32) with
     Import => True, Convention => C, External_Name => "SDL_SetColorKey";

   --------------------------
   --  Public Subprograms  --
   --------------------------
   function Load_BMP (File : in String) return SDL_Surface is
      Surface : SDL_Surface;
      RWops   : SDL_RWops;
   begin
      RWops := RW_From_File (File, "rb");
      Surface := C_SDL_LoadBMP_RW (RWops, 1);
      if Surface = null then
	 raise Surface_Error;
      end if;

      return Surface;
   end Load_BMP;

   procedure Free_Surface (Surface : in out SDL_Surface) is
   begin
      C_SDL_FreeSurface (Surface);
      Surface := null;
   end Free_Surface;

   procedure Set_Color_Key
     (Surface : in SDL_Surface;
      Flag    : in Boolean;
      Key     : in Uint32)
   is
      C_Flag : C.int;
   begin
      if Flag then
	 C_Flag := 1;
      else
	 C_Flag := 0;
      end if;
      C_SDL_SetColorKey (Surface, C_Flag, Key);
   end Set_Color_Key;
end SDL2_Surface;
