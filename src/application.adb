with SDL2; use SDL2;

package body Application is
   function Get_Audio return Audio_Driver is
   begin
      return App.Audio;
   end Get_Audio;

   function Get_Video return Video_Driver is
   begin
      return App.Video;
   end Get_Video;

   procedure Deinit is
   begin
      App.Audio.Deinit;
      App.Finalize;
   end Deinit;

   procedure Init is
   begin
      App.Audio.Init;
      App.Video.Init;
      App.Game.Start;
   end Init;

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

   procedure Render is
   begin
      Start (App.Video);
      App.Game.Render (App.Video);
      Finish (App.Video);
   end Render;

   procedure Finalize (App : in out App_Type) is
   begin
      null;
   end Finalize;
end Application;
