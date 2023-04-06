with Map;          use Map;
with Object_Map;   use Object_Map;
with SDL2;         use SDL2;
with Player;       use Player;
with Video;        use Video;
with Interfaces.C;

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

   procedure Change_Scale (Game : out Game_Type; dS : in  Interfaces.C.int);

private
   type Player_Array is array (Natural range <>) of Player_Type;

   type Game_Type is tagged limited
      record
         Map          : Map_Type;
         Objs         : Object_Map_Type;
         Human_Player : Player_Type;
         Players      : Player_Array (0 .. 20);
         Logical_Size : Interfaces.C.int := 256;
      end record;

   procedure Keyboard_Input (Game : in out Game_Type; Event : in SDL_Event);
end Game;
