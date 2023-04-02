package body Game is
   procedure Start (Game : in out Game_Type) is
   begin
      Game.Map.Create;
   end Start;

   procedure Render (Game : in out Game_Type; Video : in out Video_Driver) is
   begin
      Game.Map.Render (Video);
   end Render;

   procedure Finalize (Object : in out Game_Type) is
   begin
      null;
   end Finalize;
end Game;
