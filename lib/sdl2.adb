with Interfaces.C; use Interfaces.C;

package body SDL2 is
   package C renames Interfaces.C;

   ----------------------------
   --  External C Functions  --
   ----------------------------
   function C_SDL_Init (Flags : in Initialization_Flag) return C.int with
     Import => True, Convention => C, External_Name => "SDL_Init";

   --------------------------
   --  Public Subprograms  --
   --------------------------
   procedure Initialize (Flags : in Initialization_Flag) is
      Error : C.int;
   begin
      Error := C_SDL_Init (Flags);
      if Error < 0 then
	 raise Initialization_Error;
      end if;
   end Initialize;
end SDL2;
