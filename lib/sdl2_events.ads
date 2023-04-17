with SDL2_Stdinc; use SDL2_Stdinc;

package SDL2_Events is
   type SDL_Keysym is
      record
	 Scancode     : aliased Sint32;
	 Sym          : aliased Sint32;
	 Modification : aliased Uint16;
	 Unused       : aliased Uint32;
      end record;
   pragma Convention (C, SDL_Keysym);

   type SDL_KeyboardEvent is
      record
	 Event_Type : aliased Uint32;
	 Timestamp  : aliased Uint32;
	 WindowID   : aliased Uint32;
	 State      : aliased Uint8;
	 Repeat     : aliased Uint8;
	 Padding2   : aliased Uint8;
	 Padding3   : aliased Uint8;
	 Keysym     : aliased SDL_Keysym;
      end record;
   pragma Convention (C, SDL_KeyboardEvent);

   type Event_Padding is array (Uint32 range <>) of Uint32;
   pragma Convention (C, Event_Padding);

   type SDL_Event (T : Uint32 := 0) is
      record
	 case T is
	    when 16#0# =>
	       Event_Type : aliased Uint32;
	    when 16#300# =>
	       Key        : aliased SDL_KeyboardEvent;
	    when others =>
	       Padding    : aliased Event_Padding (1 .. 56);
	 end case;
      end record;
   pragma Unchecked_Union (SDL_Event);

   --------------------------
   --  Public Subprograms  --
   --------------------------
   procedure Wait_Event (Event : access SDL_Event);

   ------------------
   --  Exceptions  --
   ------------------
   Event_Error : exception;
end SDL2_Events;
