with Interfaces.C;
with Interfaces.C.Strings;

package body SDL2_RWops is
   package C renames Interfaces.C;

   ----------------------------
   --  External C Functions  --
   ----------------------------
   function C_SDL_RWFromFile
     (File : in C.Strings.chars_ptr;
      Mode : in C.Strings.chars_ptr)
     return SDL_RWops with
     Import => True, Convention => C, External_Name => "SDL_RWFromFile";

   --------------------------
   --  Public Subprograms  --
   --------------------------
   function RW_From_File
     (File : in String;
      Mode : in String)
     return SDL_RWops
   is
      RWops  : SDL_RWops;
      C_File : C.Strings.chars_ptr;
      C_Mode : C.Strings.chars_ptr;
   begin
      C_File := C.Strings.New_String (File);
      C_Mode := C.Strings.New_String (Mode);
      RWops := C_SDL_RWFromFile (C_File, C_Mode);
      C.Strings.Free (C_File);
      C.Strings.Free (C_Mode);
      if RWops = null then
	 raise RWops_Error;
      end if;

      return RWops;
   end RW_From_File;
end SDL2_RWops;
