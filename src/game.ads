with Map;         use Map;
with Object_Map;  use Object_Map;
with SDL2_Events; use SDL2_Events;
with SDL2_Stdinc; use SDL2_Stdinc;
with Player;      use Player;
with Video;       use Video;

package Game is
   type Game_Type is tagged limited private;

   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize (Game : in out Game_Type);
   procedure Initialize   (Game : in out Game_Type);

   ------------------------
   --  Game Subprograms  --
   ------------------------
   procedure Render (Game : in out Game_Type; Video : in out Video_Driver);

   procedure Input (Game : in out Game_Type; Event : in SDL_Event);

   procedure Change_Scale (Game : out Game_Type; dS : in Integer);

private
   type Player_Array is array (Natural range <>) of Player_Type;

   type Game_Type is tagged limited
      record
         Map          : Map_Type;
         Objs         : Object_Map_Type;
         Human_Player : Player_Type;
         Players      : Player_Array (0 .. 20);
         Logical_Size : Integer := 256;
      end record;

   procedure AI_Do_Move (G : in out Game_Type);

   procedure Generate_Objects
     (Game       : in out Game_Type;
      Spawn_Rate : in     Float);
   
   procedure Initialize_AI_Players (Game : in out Game_Type);

   procedure Keyboard_Input (Game : in out Game_Type; Event : in SDL_Event);

   procedure Move
     (Game   : in out Game_Type;
      Player : in out Player_Type;
      X      : in     Sint32;
      Y      : in     Sint32);
end Game;
