with Ada.Finalization;
with Audio;            use Audio;
with Game;             use Game;
with Video;            use Video;

package Application is
   type App_Type is new Ada.Finalization.Controlled with private;
   
   function Get_Audio return Audio_Driver;
   function Get_Video return Video_Driver;
   
   procedure Deinit;
   procedure Init;
   procedure Game_Loop;

private
   type App_Type is new Ada.Finalization.Controlled with
      record
	 Audio : Audio_Driver;
	 Video : Video_Driver;
	 Game  : Game_Type;
      end record;

   overriding procedure Finalize (App : in out App_Type);
   
   App : App_Type;

   procedure Render;
end Application;
