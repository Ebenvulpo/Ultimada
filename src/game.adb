with Ada.Text_IO;
with Application;
with Audio;        use Audio;
with Interfaces.C; use Interfaces.C;

package body Game is
   package C renames Interfaces.C;

   procedure Start (Game : in out Game_Type) is
   begin
      Game.Human_Player.Create;
      Game.Map.Create(2, 7, 2, 0.95);
      Game.Objs.Create(9, 11, 0, 0.1);
   end Start;

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
      Audio : Audio_Driver := Application.Get_Audio;
   begin
      Game.Human_Player.Get_Location (Player_Location_X, Player_Location_Y);
      case Keyboard.Keysym.Scancode is
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
      Ada.Text_IO.Put_Line (Keyboard.Keysym.Scancode'Image);
      end case;
   end Keyboard_Input;

   procedure Finalize (Game : in out Game_Type) is
   begin
      null;
   end Finalize;
end Game;
