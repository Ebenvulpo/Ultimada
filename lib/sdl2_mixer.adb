with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings;
with SDL2_RWops;           use SDL2_RWops;

package body SDL2_Mixer is
   package C renames Interfaces.C;

   ----------------------------
   --  External C Functions  --
   ----------------------------
   procedure C_Mix_FreeChunk (Chunk : in Mix_Chunk) with
     Import => True, Convention => C, External_Name => "Mix_FreeChunk";

   procedure C_Mix_FreeMusic (Music : in Mix_Music) with
     Import => True, Convention => C, External_Name => "Mix_FreeMusic";

   function C_Mix_Init (Flags : in Uint32) return C.int with
     Import => True, Convention => C, External_Name => "Mix_Init";

   function C_Mix_LoadMUS
     (File : in C.Strings.chars_ptr)
     return Mix_Music with
     Import => True, Convention => C, External_Name => "Mix_LoadMUS";

   function C_Mix_LoadWAV_RW
     (File    : in SDL_RWops;
      FreeSrc : in C.int)
     return Mix_Chunk with
     Import => True, Convention => C, External_Name => "Mix_LoadWAV_RW";

   function C_Mix_OpenAudio
     (Frequenecy : in Frequenecy_Type;
      Format     : in Format_Type;
      Channels   : in Number_Of_Channels_Type;
      Chunksize  : in Chunksize_Type)
     return C.int with
     Import => True, Convention => C, External_Name => "Mix_OpenAudio";

   function C_Mix_PlayChannel_Timed
     (Channel : in Channel_Type;
      Chunk   : in Mix_Chunk;
      Loops   : in C.int;
      Ticks   : in C.int)
     return C.int with
     Import => True, Convention => C, External_Name => "Mix_PlayChannelTimed";

   function C_Mix_Playing (Channel : in Channel_Type) return C.int with
     Import => True, Convention => C, External_Name => "Mix_Playing";

   --------------------------
   --  Public Subprograms  --
   --------------------------
   procedure Free_Chunk (Chunk : in out Mix_Chunk) is
   begin
      C_Mix_FreeChunk (Chunk);
      Chunk := null;
   end Free_Chunk;

   procedure Free_Music (Music : in out Mix_Music) is
   begin
      C_Mix_FreeMusic (Music);
      Music := null;
   end Free_Music;

   procedure Initialize (Flags : in Uint32) is
      Error : C.int;
   begin
      Error := C_Mix_Init (Flags);
      if Error < 0 then
	 raise Mixer_Error;
      end if;
   end Initialize;

   function Load_MUS (File : in String) return Mix_Music is
      Music  : Mix_Music;
      C_File : C.Strings.chars_ptr;
   begin
      C_File := C.Strings.New_String (File);
      Music := C_Mix_LoadMUS (C_File);
      C.Strings.Free (C_File);
      if Music = null then
	 raise Mixer_Error;
      end if;

      return Music;
   end Load_MUS;

   function Load_WAV (File : in String) return Mix_Chunk is
      Chunk  : Mix_Chunk;
      RWops  : SDL_RWops;
   begin
      RWops := RW_From_File (File, "rb");
      Chunk := C_Mix_LoadWAV_RW (RWops, 1);
      if Chunk = null then
	 raise Mixer_Error;
      end if;

      return Chunk;
   end Load_WAV;

   procedure Open_Audio
     (Frequenecy : in Frequenecy_Type;
      Format     : in Format_Type;
      Channels   : in Number_Of_Channels_Type;
      Chunksize  : in Chunksize_Type)
   is
      Error : C.int;
   begin
      Error := C_Mix_OpenAudio (Frequenecy, Format, Channels, Chunksize);
      if Error < 0 then
	 raise Mixer_Error;
      end if;
   end Open_Audio;

   procedure Play_Channel
     (Channel : in Channel_Type;
      Chunk   : in Mix_Chunk;
      Loops   : in Integer)
   is
      Error : C.int;
   begin
      Error := C_Mix_PlayChannel_Timed (Channel, Chunk, C.int (Loops), -1);
      if Error < 0 then
	 raise Mixer_Error;
      end if;
   end Play_Channel;

   function Playing (Channel : in Channel_Type) return Boolean is
      Is_Playing : C.int;
   begin
      Is_Playing := C_Mix_Playing (Channel);
      if Is_Playing = 1 then
	 return True;
      end if;

      return False;
   end Playing;
end SDL2_Mixer;
