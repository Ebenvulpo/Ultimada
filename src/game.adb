with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with Application;
with Audio;       use Audio;
with Object;
with Person;
with Rand;
with Tile;

package body Game is
   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize (Game : in out Game_Type) is
   begin
      Game.Map.Deinitialize;
      Game.Objs.Deinitialize;
   end Deinitialize;

   procedure Initialize (Game : in out Game_Type) is
      Location_X : Player.Location_X;
      Location_Y : Player.Location_Y;
   begin
      Game.Map.Initialize (Tile.Floor_Dirt, Tile.Tree, Tile.Floor_Grass, 0.95);
      Game.Objs.Initialize;
      Game.Generate_Objects (0.01);
      Game.Initialize_AI_Players;
      --  Randomly place the player somewhere in the world.
      loop
	 Location_X := Player.Location_X (Rand.RandomN (0, 256));
	 Location_Y := Player.Location_Y (Rand.RandomN (0, 256));

	 if Game.Map.Get_Tile (Location_X, Location_Y).Is_Walkable then
	    Game.Human_Player.Initialize
	      (ID => Person.Person_Pink,
	       X  => Location_X,
	       Y  => Location_Y);
	    goto End_Loop;
	 end if;
      end loop;
  <<End_Loop>>
   end Initialize;

   ------------------------
   --  Game Subprograms  --
   ------------------------
   procedure Change_Scale
     (Game : out Game_Type;
      dS   : in  Integer)
   is
      S : Integer;
   begin
      S := Game.Logical_Size + dS;
      if  50 <= S and S <= 600 then
         Game.Logical_Size := Game.Logical_Size + dS;
         Ada.Text_IO.Put_Line("Logical Scale " & Game.Logical_Size'Image);
      end if;
   end Change_Scale;

   procedure Render (Game : in out Game_Type; Video : in out Video_Driver) is
      H_X : Sint32;
      H_Y : Sint32;
      P_X : Sint32;
      P_Y : Sint32;
      SOX : Sint32;
      SOY : Sint32;
   begin
      Game.Human_Player.Get_Location (H_X, H_Y);

      Video.Change_Scale(Game.Logical_Size);

      SOX := (Sint32 (Game.Logical_Size) / 16) / 2;
      SOY := (Sint32 (Game.Logical_Size) / 16) / 2;

      Game.Map.Render  (Video, -H_X + SOX, -H_Y + SOY, -8, -8);
      Game.Objs.Render (Video, -H_X + SOX, -H_Y + SOY, -8, -8);

      --  AI Players
      for I in Game.Players'Range loop
	 Game.Players (I).Get_Location (P_X, P_Y);
	 Video.Draw_Person_Tile ((-H_X + SOX) + P_X, (-H_Y + SOY) + P_Y, Game.Players (I).Get_ID, -8, -8);
      end loop;

      --  Human Player
      Video.Draw_Person_Tile (SOX, SOY, 0, -8, -8);
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
   procedure AI_Do_Move (G : in out Game_Type)
   is
      package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions (Float);
      use Float_Functions;

      Distance : Float;
      H_X      : Player.Location_X'Base;
      H_Y      : Player.Location_Y'Base;
      P_X      : Player.Location_X'Base;
      P_Y      : Player.Location_Y'Base;

      A : Audio_Driver;
   begin
      A := Application.Get_Audio;

      for I in G.Players'Range loop
	 if G.Players (I).Get_ID = Person.Person_None then
	    goto End_Of_Loop;
	 end if;

	 --  The AI may "skip" a turn.
	 if Rand.RandomN (0, 4) < 1 then
	    goto End_Of_Loop;
	 end if;

	 G.Players (I).Get_Location (P_X, P_Y);
	 G.Human_Player.Get_Location (H_X, H_Y);

	 Distance := Sqrt (Float ((P_X - H_X)**2 + (P_Y - H_Y)**2));

	 --  If the AI "sees" the human player.
	 if Distance <= 10.0 then
	    if H_X > P_X and G.Map.Is_Tile_Walkable (P_X + 1, P_Y) then
	       A.Play_Sound (Monster_Sound);
	       G.Move (G.Players (I), P_X + 1, P_Y);
	    elsif H_X < P_X and G.Map.Is_Tile_Walkable (P_X - 1, P_Y) then
	       A.Play_Sound (Monster_Sound);
	       G.Move (G.Players (I), P_X - 1, P_Y);
	    elsif H_Y > P_Y and G.Map.Is_Tile_Walkable (P_X, P_Y + 1) then
	       A.Play_Sound (Monster_Sound);
	       G.Move (G.Players (I), P_X, P_Y + 1);
	    elsif G.Map.Is_Tile_Walkable (P_X, P_Y - 1) then
	       A.Play_Sound (Monster_Sound);
	       G.Move (G.Players (I), P_X, P_Y - 1);
	    else
	       --  Make noise out of frustration.
	       A.Play_Sound (Monster_Sound);
	    end if;
	 end if;
     <<End_Of_Loop>>
      end loop;
   end AI_Do_Move;

   procedure Generate_Objects
     (Game       : in out Game_Type;
      Spawn_Rate : in     Float)
   is
      R : Integer;
      F : Float;
   begin
      for Y in Map.Map_Height_Type range 0 .. Map.Map_Height_Type'Last loop
	 for X in Map.Map_Width_Type range 0 .. Map.Map_Width_Type'Last loop
	    F := Rand.randomF;

	    R := Object.Item_None;
	    if F < Spawn_Rate then
	       R := Rand.randomN (Object.Item_Chest, Object.Item_Star2);
	    end if;

	    if Object.Is_Placeable (Game.Map.Get_Tile (X, Y).Get_ID) then
	       Game.Objs.Change_Object (R, X, Y);
	    end if;
	 end loop;
      end loop;
   end Generate_Objects;

   procedure Initialize_AI_Players (Game : in out Game_Type) is
      P_I : Integer;
      X   : Player.Location_X;
      Y   : Player.Location_Y;
   begin
      for I in Game.Players'Range loop
	 P_I := Rand.RandomN (Person.Skeleton_Pink, Person.Zombie_Pink);

	 loop
	    X := Player.Location_X (Rand.RandomN (0, 256));
	    Y := Player.Location_Y (Rand.RandomN (0, 256));

	    if Game.Map.Get_Tile (X, Y).Is_Walkable then
	       Ada.Text_IO.Put ("AI X:");
	       Ada.Text_IO.Put (X'Image);
	       Ada.Text_IO.Put (" Y:");
	       Ada.Text_IO.Put_Line (Y'Image);

	       Game.Players (I).Initialize ("AI_Person", Person.Person_Type (P_I), X, Y);
	       goto Exit_Loop;
	    end if;
	 end loop;
     <<Exit_Loop>>
      end loop;
   end Initialize_AI_Players;

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
	    Audio.Play_Background_Sound (Wind_Sound);
	    Game.Move (Game.Human_Player, Player_Location_X, Player_Location_Y - 1);
	    Game.AI_Do_Move;
	 when 4 | 80 => --97, a
	    Ada.Text_IO.Put_Line ("Left");
	    Audio.Play_Background_Sound (Wind_Sound);
	    Game.Move (Game.Human_Player, Player_Location_X - 1, Player_Location_Y);
	    Game.AI_Do_Move;
	 when 22 | 81 => --115, s
	    Ada.Text_IO.Put_Line ("Down");
	    Audio.Play_Background_Sound (Wind_Sound);
	    Game.Move (Game.Human_Player, Player_Location_X, Player_Location_Y + 1);
	    Game.AI_Do_Move;
	 when 7 | 79 => --100, d
	    Ada.Text_IO.Put_Line ("Right");
	    Audio.Play_Background_Sound (Wind_Sound);
	    Game.Move (Game.Human_Player, Player_Location_X + 1, Player_Location_Y);
	    Game.AI_Do_Move;
	 when 45 => -- "-"
	    Game.Change_Scale(32);
	 when 46 => -- "+"
	    Game.Change_Scale(-32);
	    --   when others => null;
	 when others => --added for debugging
	    Ada.Text_IO.Put_Line (Event.Key.Keysym.Scancode'Image);
      end case;
   end Keyboard_Input;

   procedure Move
     (Game   : in out Game_Type;
      Player : in out Player_Type;
      X      : in     Sint32;
      Y      : in     Sint32)
   is
      Map_Bound_X : constant Sint32 := Map_Width_Type'Last;
      Map_Bound_Y : constant Sint32 := Map_Height_Type'Last;
   begin
      if
	0 <= X and X <= Map_Bound_X and
	0 <= Y and Y <= Map_Bound_Y
      then
	 if Game.Map.Get_Tile (X, Y).Is_Walkable then
	    Player.Change_Location (X, Y);

	    if Game.Objs.Get_Item (X, Y) /= Object.Item_None then
	       Game.Objs.Change_Object (Object.Item_None, X, Y);
	    end if;
	 end if;
      end if;
      Ada.Text_IO.Put_Line (X'Image);
      Ada.Text_IO.Put_Line (Y'Image);
   end Move;
end Game;
