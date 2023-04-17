with Ada.Text_IO;
with Application;
with Filepath;
with SDL2;        use SDL2;
with Version;

procedure Ultimada is
begin
   SDL2.Initialize (SDL_Init_Audio or SDL_Init_Video or SDL_Init_Events);

   Filepath.Initialize;

   Ada.Text_IO.Put_Line ("Hello World Ultimada (" & Version.Get & ")!");
   Ada.Text_IO.Put_Line ("Controls:");
   Ada.Text_IO.Put_Line ("WASD, Arross, -/+");
   Ada.Text_IO.New_Line;

   Application.Initialize;
   Application.Game_Loop;
   Application.Deinitialize;

   SDL2.Deinitialize;
end Ultimada;
