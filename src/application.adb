with SDL2; use SDL2;

package body Application is
   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize is
   begin
      App.Audio.Deinitialize;
      App.Video.Deinitialize;
      App.Game.Deinitialize;
   end Deinitialize;

   procedure Initialize is
   begin
      App.Audio.Initialize;
      App.Video.Initialize;
      App.Game.Initialize;
   end Initialize;

   -------------------------------
   --  Application Subprograms  --
   -------------------------------
   function Get_Audio return Audio_Driver is
   begin
      return App.Audio;
   end Get_Audio;

   function Get_Video return Video_Driver is
   begin
      return App.Video;
   end Get_Video;

   procedure Game_Loop is
      Event : aliased SDL_Event;
   begin
      loop
	 SDL_WaitEvent (Event'Access);
	 case Event.Event_Type is
	    when 16#100# => goto Exit_Loop;
	    when others  => null;
	 end case;

	 App.Game.Input (Event);
	 Render;
      end loop;
  <<Exit_Loop>>
      return;
   end Game_Loop;

   ---------------------------
   --  Private Subprograms  --
   ---------------------------
   procedure Render is
   begin
      Start (App.Video);
      App.Game.Render (App.Video);
      Finish (App.Video);
   end Render;
end Application;
