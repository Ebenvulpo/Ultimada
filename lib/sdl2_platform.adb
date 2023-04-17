with Interfaces.C.Strings; use Interfaces.C.Strings;

package body SDL2_Platform is
   ----------------------------
   --  External C Functions  --
   ----------------------------
   function C_SDL_GetPlatform return Interfaces.C.Strings.chars_ptr with
     Import => True, Convention => C, External_Name => "SDL_GetPlatform";

   --------------------------
   --  Public Subprograms  --
   --------------------------
   function Get_Platform return String is
   begin
      return Value (C_SDL_GetPlatform);
   end Get_Platform;
end SDL2_Platform;
