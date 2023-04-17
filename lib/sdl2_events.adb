with Interfaces.C; use Interfaces.C;

package body SDL2_Events is
   package C renames Interfaces.C;

   ----------------------------
   --  External C Functions  --
   ----------------------------
   function C_SDL_WaitEvent (Event : access SDL_Event) return C.int with
     Import => True, Convention => C, External_Name => "SDL_WaitEvent";

   --------------------------
   --  Public Subprograms  --
   --------------------------
   procedure Wait_Event (Event : access SDL_Event) is
      Error : C.int;
   begin
      Error := C_SDL_WaitEvent (Event);
      if Error = 0 then
	 raise Event_Error;
      end if;
   end Wait_Event;
end SDL2_Events;
