with Ada.Finalization;
with Map;              use Map;
with Player;           use Player;
with Video;            use Video;

package Game is
   type Game_Type is new Ada.Finalization.Controlled with private;
   
   procedure Start (Game : in out Game_Type);
   procedure Render (Game : in out Game_Type; Video : in out Video_Driver);

private
   type Player_Array is array (Natural range <>) of Player_Type;

   type Game_Type is new Ada.Finalization.Controlled with
      record
	 Map          : Map_Type;
	 Human_Player : Player_Type;
	 Players      : Player_Array (0 .. 20);
      end record;

   overriding procedure Finalize (Object : in out Game_Type);
end Game;
