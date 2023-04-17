with SDL2_Stdinc; use SDL2_Stdinc;

package SDL2 is
   type Initialization_Flag is new Uint32;

   -----------------
   --  Constants  --
   -----------------
   SDL_Init_Timer           : constant Initialization_Flag := 16#0000_0001#;
   SDL_Init_Audio           : constant Initialization_Flag := 16#0000_0010#;
   SDL_Init_Video           : constant Initialization_Flag := 16#0000_0020#;
   SDL_Init_Joystick        : constant Initialization_Flag := 16#0000_0200#;
   SDL_Init_Haptic          : constant Initialization_Flag := 16#0000_1000#;
   SDL_Init_Game_Controller : constant Initialization_Flag := 16#0000_2000#;
   SDL_Init_Events          : constant Initialization_Flag := 16#0000_4000#;
   SDL_Init_Sensor          : constant Initialization_Flag := 16#0000_8000#;

   --------------------------
   --  Public Subprograms  --
   --------------------------
   procedure Deinitialize with
     Import => True, Convention => C, External_Name => "SDL_Quit";

   procedure Initialize (Flags : in Initialization_Flag);

   ------------------
   --  Exceptions  --
   ------------------
   Initialization_Error : exception;
end SDL2;
