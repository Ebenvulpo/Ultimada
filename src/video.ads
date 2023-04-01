with Ada.Finalization;
with Interfaces.C;     use Interfaces.C;
with SDL2;             use SDL2;

package Video is
   type Video_Driver is new Ada.Finalization.Controlled with private;

   procedure Start  (Video : in out Video_Driver);
   procedure Finish (Video : in out Video_Driver);

   procedure Draw_Rectangle
     (Video   : in out Video_Driver;
      W, H    : in     C.int;
      X, Y    : in     C.int;
      R, G, B : in     C.unsigned_char);

   procedure Init (Video : in out Video_Driver);

private
   type Texture_Array is array (Natural range <>) of SDL_Texture;

   type Video_Driver is new Ada.Finalization.Controlled with
      record
	 Window   : SDL_Window   := null;
	 Renderer : SDL_Renderer := null;
	 Textures : Texture_Array (0 .. 64) := (others => null);
      end record;

   overriding procedure Finalize (Object : in out Video_Driver);
end Video;
