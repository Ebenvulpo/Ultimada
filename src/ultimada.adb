with Ada.Text_IO;
with Application;  use Application;
with Interfaces.C; use Interfaces.C;
with SDL2;         use SDL2;

procedure Ultimada is
   App   : App_Type;
   Error : C.int;
begin
   Error := SDL_Init (16#0000_0020# or 16#0000_4000# or 16#0000_0010#);
   if Error < 0 then
      raise Program_Error;
   end if;

   Ada.Text_IO.Put_Line ("Hello World Ultimada!");

   App.Init;
   App.Game_Loop;
   App.Finalize;

   SDL_Quit;
end Ultimada;
