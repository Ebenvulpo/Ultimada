with Ada.Strings.Bounded;
with Interfaces.C;        use Interfaces.C;
with SDL2;                use SDL2;

package Video is
   type Video_Driver is tagged private;

   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize (Video : in out Video_Driver);
   procedure Initialize   (Video : in out Video_Driver);

   -------------------------
   --  Video Subprograms  --
   -------------------------
   procedure Start  (Video : in out Video_Driver);
   procedure Finish (Video : in out Video_Driver);

   procedure Draw_Rectangle
     (Video   : in out Video_Driver;
      W, H    : in     C.int;
      X, Y    : in     C.int;
      R, G, B : in     C.unsigned_char);

   procedure Draw_Tile
     (Video          : in out Video_Driver;
      X, Y           : in     C.int;
      Number         : in     Natural;
      Pixel_Offset_X : in     C.int;
      Pixel_Offset_Y : in     C.int);

   procedure Change_scale
     (Video   : in out Video_Driver;
      S       : in     C.int);

private
   type Texture_Array is array (Natural range <>) of SDL_Texture;

   type Video_Driver is tagged
      record
	 Window   : SDL_Window   := null;
	 Renderer : SDL_Renderer := null;
	 Textures : Texture_Array (0 .. 64) := (others => null);
      end record;

   package SB is new Ada.Strings.Bounded.Generic_Bounded_Length (64);

   type Bitmap_Array_Type is array (Natural range <>) of SB.Bounded_String;
   Bitmap_Array : constant Bitmap_Array_Type :=
     (
      SB.To_Bounded_String ("void_pink.bmp"),
      SB.To_Bounded_String ("chest.bmp"),            SB.To_Bounded_String ("floor_dirt.bmp"),
      SB.To_Bounded_String ("floor_grass.bmp"),      SB.To_Bounded_String ("floor_grass2.bmp"),
      SB.To_Bounded_String ("floor_grass3.bmp"),     SB.To_Bounded_String ("floor_ice.bmp"),
      SB.To_Bounded_String ("floor_lava.bmp"),       SB.To_Bounded_String ("floor_lava2.bmp"),
      SB.To_Bounded_String ("floor_lava3.bmp"),      SB.To_Bounded_String ("gem_amethyst_pink.bmp"),
      SB.To_Bounded_String ("gem_emerald_pink.bmp"), SB.To_Bounded_String ("gem_ruby_pink.bmp"),
      SB.To_Bounded_String ("log_pink.bmp"),         SB.To_Bounded_String ("person_pink.bmp"),
      SB.To_Bounded_String ("rock_pink.bmp"),        SB.To_Bounded_String ("skeleton_pink.bmp"),
      SB.To_Bounded_String ("skeleton2_pink.bmp"),   SB.To_Bounded_String ("slime1.bmp"),
      SB.To_Bounded_String ("slime1_pink.bmp"),      SB.To_Bounded_String ("slime2.bmp"),
      SB.To_Bounded_String ("snowball_pink.bmp"),    SB.To_Bounded_String ("star_pink.bmp"),
      SB.To_Bounded_String ("star2_pink.bmp"),       SB.To_Bounded_String ("tree.bmp"),
      SB.To_Bounded_String ("tree_pink.bmp"),        SB.To_Bounded_String ("zombie_pink.bmp"));

   procedure Load_Textures (Video : in out Video_Driver);
end Video;
