with Ada.Text_IO;
with Filepath;
with Interfaces.C; use Interfaces.C;

package body Audio is
   package C renames Interfaces.C;

   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize (Audio : in out Audio_Driver) is
   begin
      for I in Audio.Waves'Range loop
	 if Audio.Waves (I) /= null then
	    Mix_FreeChunk (Audio.Waves (I));
	 end if;
      end loop;

      Mix_Quit;
   end Deinitialize;

   procedure Initialize (Audio : in out Audio_Driver) is
      Error : C.int;
   begin
      Ada.Text_IO.Put_Line ("Staring Audio Driver");

      Error := Mix_Init (16#0000_0008#);
      if Error = 0 then
     	 raise Program_Error;
      end if;

      Error := Mix_OpenAudio (44100, 16#8010#, 8, 1024);
      if Error < 0 then
	 raise Program_Error;
      end if;

      Load_Audio_Files (Audio);

      Ada.Text_IO.New_Line;
   end Initialize;

   -------------------------
   --  Audio Subprograms  --
   -------------------------
   procedure Play_Background_Sound
     (Audio : in out Audio_Driver;
      Sound : in     Sound_ID_Type)
   is
      Error : C.int;
   begin
      if Mix_Playing (1) > 0 then
	 return;
      end if;

      Error := Mix_PlayChannel (1, Audio.Waves (Sound), 0);
      if Error < 0 then
	 raise Program_Error;
      end if;
   end Play_Background_Sound;

   procedure Play_Sound
     (Audio : in out Audio_Driver;
      Sound : in     Sound_ID_Type)
   is
      Error : C.Int;
   begin
      Error := Mix_PlayChannel (0, Audio.Waves (Sound), 0);
      if Error < 0 then
	 raise Program_Error;
      end if;
   end Play_Sound;

   ---------------------------
   --  Private Subprograms  --
   ---------------------------
   procedure Load_Audio_Files (Audio : in out Audio_Driver) is
      Chunk : Mix_Chunk;
   begin
      Ada.Text_IO.Put_Line ("Loading Audio Files...");
      for I in Sound_Array'Range loop
	 Ada.Text_IO.Put      ("Loading: ");
	 Ada.Text_IO.Put_Line (SB.To_String (Sound_Array (I).Name));

	 Chunk := Mix_LoadWAV (Filepath.Get (SB.To_String (Sound_Array (I).Name), "wavs"));
	 if Chunk = null then
	    raise Program_Error;
	 end if;

	 if Audio.Waves (Sound_Array (I).Number) /= null then
	    raise Program_Error;
	 end if;

	 Audio.Waves (Sound_Array (I).Number) := Chunk;
      end loop;
   end Load_Audio_Files;
end Audio;
