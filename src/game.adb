with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;

package body Game is
   package C renames Interfaces.C;

   procedure Start (Game : in out Game_Type) is
   begin
      Game.Human_Player.Create;
      Game.Map.Create;
   end Start;

   procedure Render (Game : in out Game_Type; Video : in out Video_Driver) is
      Player_Location_X : C.int;
      Player_Location_Y : C.int;
   begin
      Game.Human_Player.Get_Location (Player_Location_X, Player_Location_Y);
      Game.Map.Render (Video, (-Player_Location_X + 20), (-Player_Location_Y + 15));
      Video.Draw_Tile (20, 15, 13);
   end Render;
   
   procedure Input (Game : in out Game_Type; Event : in SDL_Event) is
      Keyboard_Event    : SDL_KeyboardEvent;
   begin
      case Event.Event_Type is
	 --  SDL_Keydown
	 when 16#300# =>
	    Ada.Text_IO.Put_Line ("Keyboard_Event");
	    Keyboard_Event := Get_KeyboardEvent (Event);
	    Keyboard_Input (Game, Keyboard_Event);
	 when others => null;
      end case;
   end Input;

   procedure Keyboard_Input (Game : in out Game_Type; Keyboard : in SDL_KeyboardEvent) is
      Player_Location_X : Location_X;
      Player_Location_Y : Location_Y;
   begin
      Game.Human_Player.Get_Location (Player_Location_X, Player_Location_Y);
      case Keyboard.Keysym.Scancode is
	 when 4  =>
	    Ada.Text_IO.Put_Line ("Left");
	    Game.Human_Player.Change_Location (Player_Location_X - 1, Player_Location_Y);
	 when 7  =>
	    Ada.Text_IO.Put_Line ("Right");
	    Game.Human_Player.Change_Location (Player_Location_X + 1, Player_Location_Y);
	 when 22 =>
	    Ada.Text_IO.Put_Line ("Down");
	    Game.Human_Player.Change_Location (Player_Location_X, Player_Location_Y + 1);
	 when 26 =>
	    Ada.Text_IO.Put_Line ("Up");
	    Game.Human_Player.Change_Location (Player_Location_X, Player_Location_Y - 1);
	 when others => null;
      end case;
   end Keyboard_Input;

   procedure Finalize (Game : in out Game_Type) is
   begin
      null;
   end Finalize;
end Game;
