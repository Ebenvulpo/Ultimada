with Ada.Finalization;
with SDL2;             use SDL2;
with System;           use System;

package Video is
   type Video_Driver is new Ada.Finalization.Controlled with private;

   procedure Start  (Video : in out Video_Driver);
   procedure Finish (Video : in out Video_Driver);

   procedure Init (Video : in out Video_Driver);

private
   type Video_Driver is new Ada.Finalization.Controlled with
      record
	 Window   : SDL_Window   := SDL_Window (System.Null_Address);
	 Renderer : SDL_Renderer := SDL_Renderer (System.Null_Address);
      end record;

   overriding procedure Finalize (Object : in out Video_Driver);
end Video;
