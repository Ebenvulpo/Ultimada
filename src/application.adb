with SDL2; use SDL2;

package body Application is
   procedure Init (App : in out App_Type) is
   begin
      App.Video.Init;
      App.Game.Start;
   end Init;

   procedure Game_Loop (App : in out App_Type) is
      Event : aliased SDL_Event;
   begin
      loop
	 SDL_WaitEvent (Event'Access);
	 case Event.Event_Type is
	    when 16#100# => goto Exit_Loop;
	    when others  => null;
	 end case;

	 Render (App);
      end loop;
  <<Exit_Loop>>
      return;
   end Game_Loop;

   procedure Render (App : in out App_Type) is
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
