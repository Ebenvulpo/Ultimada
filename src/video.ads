with Ada.Strings.Bounded;
with Interfaces.C;        use Interfaces.C;
with Object;
with Person;
with SDL2;                use SDL2;
with Tile;

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

   procedure Draw_Map_Tile
     (Video          : in out Video_Driver;
      X, Y           : in     C.int;
      Number         : in     Tile.Tile_ID_Type;
      Pixel_Offset_X : in     C.int;
      Pixel_Offset_Y : in     C.int);

   procedure Draw_Object_Tile
     (Video          : in out Video_Driver;
      X, Y           : in     C.int;
      Number         : in     Object.Item_Type;
      Pixel_Offset_X : in     C.int;
      Pixel_Offset_Y : in     C.int);

   procedure Draw_Person_Tile
     (Video          : in out Video_Driver;
      X, Y           : in     C.int;
      Number         : in     Person.Person_Type;
      Pixel_Offset_X : in     C.int;
      Pixel_Offset_Y : in     C.int);

   procedure Change_Scale
     (Video   : in out Video_Driver;
      S       : in     C.int);

private
   type Object_Texture_Array is array (Object.Item_Type   range <>) of SDL_Texture;
   type Person_Texture_Array is array (Person.Person_Type range <>) of SDL_Texture;
   type Tile_Texture_Array   is array (Tile.Tile_ID_Type  range <>) of SDL_Texture;

   type Video_Driver is tagged
      record
	 Window          : SDL_Window   := null;
	 Renderer        : SDL_Renderer := null;
	 Object_Textures : Object_Texture_Array (0 .. 64) := (others => null);
	 Person_Textures : Person_Texture_Array (0 .. 64) := (others => null);
	 Tile_Textures   : Tile_Texture_Array   (0 .. 64) := (others => null);
      end record;

   package SB is new Ada.Strings.Bounded.Generic_Bounded_Length (64);

   ------------------------
   --  Map Tile Bitmaps  --
   ------------------------
   type Tile_BMP_Type is
      record
	 Name   : SB.Bounded_String := SB.To_Bounded_String ("None");
	 Number : Tile.Tile_ID_Type := 0;
      end record;

   type Tile_BMP_Array_Type is array (Tile.Tile_ID_Type range <>) of Tile_BMP_Type;
   Tile_BMP_Array : constant Tile_BMP_Array_Type :=
     (Tile_BMP_Type'(SB.To_Bounded_String ("floor_dirt.bmp"),   Tile.Floor_Dirt),
      Tile_BMP_Type'(SB.To_Bounded_String ("floor_grass.bmp"),  Tile.Floor_Grass),
      Tile_BMP_Type'(SB.To_Bounded_String ("floor_grass2.bmp"), Tile.Floor_Grass2),
      Tile_BMP_Type'(SB.To_Bounded_String ("floor_grass3.bmp"), Tile.Floor_Grass3),
      Tile_BMP_Type'(SB.To_Bounded_String ("floor_ice.bmp"),    Tile.Floor_Ice),
      Tile_BMP_Type'(SB.To_Bounded_String ("floor_lava.bmp"),   Tile.Floor_Lava),
      Tile_BMP_Type'(SB.To_Bounded_String ("floor_lava2.bmp"),  Tile.Floor_Lava2),
      Tile_BMP_Type'(SB.To_Bounded_String ("floor_lava3.bmp"),  Tile.Floor_Lava3),
      Tile_BMP_Type'(SB.To_Bounded_String ("rock_pink.bmp"),    Tile.Rock_Pink),
      Tile_BMP_Type'(SB.To_Bounded_String ("tree.bmp"),         Tile.Tree),
      Tile_BMP_Type'(SB.To_Bounded_String ("tree_pink.bmp"),    Tile.Tree_Pink));

   ----------------------
   --  Person Bitmaps  --
   ----------------------
   type Person_BMP_Type is
      record
	 Name   : SB.Bounded_String  := SB.To_Bounded_String ("None");
	 Number : Person.Person_Type := 0;
      end record;

   type Person_BMP_Array_Type is array (Person.Person_Type range <>) of Person_BMP_Type;
   Person_BMP_Array : constant Person_BMP_Array_Type :=
     (Person_BMP_Type'(SB.To_Bounded_String ("person_pink.bmp"),    Person.Person_Pink),
      Person_BMP_Type'(SB.To_Bounded_String ("skeleton_pink.bmp"),  Person.Skeleton_Pink),
      Person_BMP_Type'(SB.To_Bounded_String ("skeleton2_pink.bmp"), Person.Skeleton2_Pink),
      Person_BMP_Type'(SB.To_Bounded_String ("slime1.bmp"),         Person.Slime1),
      Person_BMP_Type'(SB.To_Bounded_String ("slime2.bmp"),         Person.Slime2),
      Person_BMP_Type'(SB.To_Bounded_String ("zombie_pink.bmp"),    Person.Zombie_Pink));

   ----------------------
   --  Object Bitmaps  --
   ----------------------
   type Object_BMP_Type is
      record
	 Name   : SB.Bounded_String := SB.To_Bounded_String ("None");
	 Number : Object.Item_Type  := Object.Item_None;
      end record;

   type Object_BMP_Array_Type is array (Object.Item_Type range <>) of Object_BMP_Type;
   Object_BMP_Array : constant Object_BMP_Array_Type :=
     (Object_BMP_Type'(SB.To_Bounded_String ("chest.bmp"),             Object.Item_Chest),
      Object_BMP_Type'(SB.To_Bounded_String ("gem_amethyst_pink.bmp"), Object.Item_Gem_Amethyst),
      Object_BMP_Type'(SB.To_Bounded_String ("gem_emerald_pink.bmp"),  Object.Item_Gem_Emerald),
      Object_BMP_Type'(SB.To_Bounded_String ("gem_ruby_pink.bmp"),     Object.Item_Gem_Ruby),
      Object_BMP_Type'(SB.To_Bounded_String ("log_pink.bmp"),          Object.Item_Log),
      Object_BMP_Type'(SB.To_Bounded_String ("snowball_pink.bmp"),     Object.Item_Snowball),
      Object_BMP_Type'(SB.To_Bounded_String ("star_pink.bmp"),         Object.Item_Star),
      Object_BMP_Type'(SB.To_Bounded_String ("star2_pink.bmp"),        Object.Item_Star2));

   ---------------------------
   --  Private Subprograms  --
   ---------------------------
   procedure Load_Textures (Video : in out Video_Driver);
   procedure Load_Objects  (Video : in out Video_Driver);
   procedure Load_Persons  (Video : in out Video_Driver);
   procedure Load_Tiles    (Video : in out Video_Driver);
end Video;
