package body SDL2 is
   function SDL_LoadBMP (File : in String) return SDL_Surface is
      function SDL_RWFromFile (File : in System.Address; Mode : in C.char_array) return System.Address
	with Import => True, Convention => C, External_Name => "SDL_RWFromFile";

      function SDL_LoadBMP_RW (File : in System.Address; FreeSrc : in C.int) return SDL_Surface
	with Import => True, Convention => C, External_Name => "SDL_LoadBMP_RW";

      File_Address :         System.Address;
      Surface      :         SDL_Surface;
      FilePath     : aliased C.char_array := C.To_C (File) & C.nul;
   begin
      File_Address := SDL_RWFromFile (FilePath'Address, "rb");
      if File_Address = System.Null_Address then
	 raise Program_Error;
      end if;

      Surface := SDL_LoadBMP_RW (File_Address, 1);
      if Surface = null then
	 raise Program_Error;
      end if;

      return Surface;
   end SDL_LoadBMP;
end SDL2;
