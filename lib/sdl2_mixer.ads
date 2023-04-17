with SDL2_Stdinc; use SDL2_Stdinc;

package SDL2_Mixer is
   type Dummy_Mix_Chunk is limited private;
   type Dummy_Mix_Music is limited private;

   type Mix_Chunk is access Dummy_Mix_Chunk;
   type Mix_Music is access Dummy_Mix_Music;
   pragma Convention (C, Mix_Chunk);
   pragma Convention (C, Mix_Music);

   type Frequenecy_Type         is new Sint32;
   type Format_Type             is new Uint16;
   type Channel_Type            is new Sint32;
   type Chunksize_Type          is new Sint32;
   type Number_Of_Channels_Type is new Sint32;

   --------------------------
   --  Public Subprograms  --
   --------------------------
   procedure Deinitialize with
     Import => True, Convention => C, External_Name => "Mix_Quit";

   procedure Free_Chunk (Chunk : in out Mix_Chunk);
   procedure Free_Music (Music : in out Mix_Music);

   procedure Initialize (Flags : in Uint32);

   function Load_MUS (File : in String) return Mix_Music;
   function Load_WAV (File : in String) return Mix_Chunk;

   procedure Open_Audio
     (Frequenecy : in Frequenecy_Type;
      Format     : in Format_Type;
      Channels   : in Number_Of_Channels_Type;
      Chunksize  : in Chunksize_Type);

   procedure Play_Channel
     (Channel : in Channel_Type;
      Chunk   : in Mix_Chunk;
      Loops   : in Integer);

   function Playing (Channel : in Channel_Type) return Boolean;

   ------------------
   --  Exceptions  --
   ------------------
   Mixer_Error : exception;

private
   type Dummy_Mix_Chunk is null record;
   type Dummy_Mix_Music is null record;
   pragma Convention (C, Dummy_Mix_Chunk);
   pragma Convention (C, Dummy_Mix_Music);
end SDL2_Mixer;
