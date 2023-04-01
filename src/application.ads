with Ada.Finalization;
with SDL2;             use SDL2;
with Video;            use Video;

package Application is
   type App_Type is new Ada.Finalization.Controlled with private;

   procedure Init (App : in out App_Type);
   procedure Game_Loop (App : in out App_Type);

private
   type App_Type is new Ada.Finalization.Controlled with
      record
	 Video : Video_Driver;
      end record;

   procedure Render (App : in out App_Type);
end Application;
