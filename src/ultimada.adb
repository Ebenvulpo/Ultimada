with Ada.Text_IO;
with Application;
with Filepath;
with Interfaces.C; use Interfaces.C;
with SDL2;         use SDL2;

procedure Ultimada is
   package C renames Interfaces.C;

   Error : C.int;
begin
   Error := SDL_Init (16#0000_0020# or 16#0000_4000# or 16#0000_0010#);
   if Error < 0 then
      raise Program_Error;
   end if;

   Filepath.Initialize;

   Ada.Text_IO.Put_Line ("Hello World Ultimada!");
   Ada.Text_IO.Put_Line ("Controls:");
   Ada.Text_IO.Put_Line ("WASD, Arross, -/+");
   Ada.Text_IO.New_Line;

   Application.Initialize;
   Application.Game_Loop;
   Application.Deinitialize;

   Filepath.Deinitialize;

   SDL_Quit;
end Ultimada;
