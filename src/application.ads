with Ada.Finalization;
with Game;                 use Game;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Video;                use Video;

package Application is
   package C renames Interfaces.C;

   type App_Type is new Ada.Finalization.Controlled with private;

   procedure Init (App : in out App_Type);
   procedure Game_Loop (App : in out App_Type);

private
   type App_Type is new Ada.Finalization.Controlled with
      record
	 Video   : Video_Driver;
	 Game    : Game_Type;
	 AppPath : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      end record;

   overriding procedure Finalize (App : in out App_Type);

   procedure Render (App : in out App_Type);
end Application;
