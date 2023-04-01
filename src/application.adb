with SDL2; use SDL2;

package body Application is
   procedure Init (App : in out App_Type) is
   begin
      Init (App.Video);
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
      App.Video.Draw_Rectangle (64, 64, 0, 0, 16#FF#, 16#FF#, 16#FF#);
      Finish (App.Video);
   end Render;
end Application;
