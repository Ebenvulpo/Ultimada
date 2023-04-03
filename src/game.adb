with Ada.Text_IO;
with Application;
with Audio;        use Audio;
with Interfaces.C; use Interfaces.C;

package body Game is
   package C renames Interfaces.C;

   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize (Game : in out Game_Type) is
   begin
      Game.Map.Deinitialize;
   end Deinitialize;

   procedure Initialize (Game : in out Game_Type) is
   begin
      Game.Human_Player.Initialize;
      Game.Map.Initialize (2, 7, 2, 0.95);
      Game.Objs.Initialize (9, 11, 0, 0.1);
   end Initialize;

   ------------------------
   --  Game Subprograms  --
   ------------------------
   procedure Change_Scale (Game : out Game_Type; dS : in  C.int) is
      s : C.int;
   begin
      s := Game.Logical_Size + dS;
      if  50 <= s and s <= 600 then
         Game.Logical_Size := Game.Logical_Size + dS;
         Ada.Text_IO.Put_Line( "Logical Scale " & Game.Logical_Size'Image );
      end if;
   end Change_Scale;

   procedure Render (Game : in out Game_Type; Video : in out Video_Driver) is
      Player_Location_X : C.int;
      Player_Location_Y : C.int;
   begin
      Game.Human_Player.Get_Location (Player_Location_X, Player_Location_Y);
      Video.Change_Scale(Game.Logical_Size);
      Game.Map.Render (Video, (-Player_Location_X + 2), (-Player_Location_Y + 2));
      Game.Objs.Render (Video, (-Player_Location_X + 2), (-Player_Location_Y + 2));
      Video.Draw_Tile (2, 2, 14);
   end Render;

   procedure Input (Game : in out Game_Type; Event : in SDL_Event) is
   begin
      case Event.Event_Type is
	 --  SDL_Keydown
	 when 16#300# =>
	    Ada.Text_IO.Put_Line ("Keyboard_Event");
	    Keyboard_Input (Game, Event);
	 when others => null;
      end case;
   end Input;

   ---------------------------
   --  Private Subprograms  --
   ---------------------------
   procedure Keyboard_Input (Game : in out Game_Type; Event : in SDL_Event) is
      Player_Location_X : Location_X;
      Player_Location_Y : Location_Y;
      Audio : Audio_Driver := Application.Get_Audio;
   begin
      Game.Human_Player.Get_Location (Player_Location_X, Player_Location_Y);
      case Event.Key.Keysym.Scancode is
	 --  wasd, arrows (uldr)
	 when 26 | 82 => --119, w
	    Ada.Text_IO.Put_Line ("Up");
	    Audio.Play_Sound (17);
	    Game.Human_Player.Change_Location (Player_Location_X, Player_Location_Y - 1);
	 when 4 | 80 => --97, a
	    Ada.Text_IO.Put_Line ("Left");
	    Audio.Play_Sound (17);
	    Game.Human_Player.Change_Location (Player_Location_X - 1, Player_Location_Y);
	 when 22 | 81 => --115, s
	    Ada.Text_IO.Put_Line ("Down");
	    Audio.Play_Sound (17);
	    Game.Human_Player.Change_Location (Player_Location_X, Player_Location_Y + 1);
	 when 7 | 79 => --100, d
	    Ada.Text_IO.Put_Line ("Right");
	    Audio.Play_Sound (17);
	    Game.Human_Player.Change_Location (Player_Location_X + 1, Player_Location_Y);
	 when 45 => -- "-"
	    Game.Change_Scale(16);
	 when 46 => -- "+"
	    Game.Change_Scale(-16);
	    --   when others => null;
	 when others => --added for debugging
	    Ada.Text_IO.Put_Line (Event.Key.Keysym.Scancode'Image);
      end case;
   end Keyboard_Input;
end Game;
