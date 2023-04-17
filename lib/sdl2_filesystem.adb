with Ada.Strings.Bounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body SDL2_Filesystem is
   -----------------------------
   --  External C Functionss  --
   -----------------------------
   function C_SDL_GetBasePath return Interfaces.C.Strings.chars_ptr with
     Import => True, Convention => C, External_Name => "SDL_GetBasePath";

   --------------------------
   --  Public Subprograms  --
   --------------------------
   function Get_Base_Path return String is
      package SB is new Ada.Strings.Bounded.Generic_Bounded_Length (10_000);

      C_String : Interfaces.C.Strings.chars_ptr;
      Path     : SB.Bounded_String;
   begin
      C_String := C_SDL_GetBasePath;
      Path := SB.To_Bounded_String (Value (C_String));
      Interfaces.C.Strings.Free (C_String);

      return SB.To_String (Path);
   end Get_Base_Path;
end SDL2_Filesystem;
