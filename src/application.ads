with Audio; use Audio;
with Game;  use Game;
with Video; use Video;

package Application is
   type App_Type is tagged private;

   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize;
   procedure Initialize;

   -------------------------------
   --  Application Subprograms  --
   -------------------------------
   function Get_Audio return Audio_Driver;
   function Get_Video return Video_Driver;

   procedure Game_Loop;

private
   type App_Type is tagged
      record
	 Audio : Audio_Driver;
	 Video : Video_Driver;
	 Game  : Game_Type;
      end record;

   App : App_Type;

   procedure Render;
end Application;
