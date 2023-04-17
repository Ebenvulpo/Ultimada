with Interfaces.C;
with Interfaces.C.Strings;

package body SDL2_Video is
   package C renames Interfaces.C;

   ----------------------------
   --  External C Functions  --
   ----------------------------
   function C_SDL_CreateWindow
     (Title      : in C.Strings.chars_ptr;
      X, Y, W, H : in C.int;
      Flags      : in Uint32)
     return SDL_Window with
     Import => True, Convention => C, External_Name => "SDL_CreateWindow";

   procedure C_SDL_DestroyWindow (Window : in out SDL_Window) with
     Import => True, Convention => C, External_Name => "SDL_DestroyWindow";

   --------------------------
   --  Public Subprograms  --
   --------------------------
   function Create_Window
     (Title  : in String;
      Width  : in Window_Width_Type;
      Height : in Window_Height_Type)
     return SDL_Window
   is
      Window  : SDL_Window;
      C_Title : C.Strings.chars_ptr;
   begin
      C_Title := C.Strings.New_String (Title);
      Window := C_SDL_CreateWindow
	(Title => C_Title,
	 X     => 200,
	 Y     => 200,
	 W     => C.int (Width),
	 H     => C.int (Height),
	 Flags => 0);
      C.Strings.Free (C_Title);
      if Window = null then
	 raise Window_Error;
      end if;

      return Window;
   end Create_Window;

   procedure Destroy_Window (Window : in out SDL_Window) is
   begin
      C_SDL_DestroyWindow (Window);
      Window := null;
   end Destroy_Window;
end SDL2_Video;
