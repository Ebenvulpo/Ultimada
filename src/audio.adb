with Ada.Text_IO;
with Filepath;

package body Audio is
   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize (Audio : in out Audio_Driver) is
   begin
      for I in Audio.Waves'Range loop
	 if Audio.Waves (I) /= null then
	    SDL2_Mixer.Free_Chunk (Audio.Waves (I));
	 end if;
      end loop;

      SDL2_Mixer.Deinitialize;
   end Deinitialize;

   procedure Initialize (Audio : in out Audio_Driver) is
   begin
      Ada.Text_IO.Put_Line ("Staring Audio Driver");

      SDL2_Mixer.Initialize (16#0000_0008#);
      SDL2_Mixer.Open_Audio (44100, 16#8010#, 8, 1024);

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
   begin
      if SDL2_Mixer.Playing (1) then
	 return;
      end if;

      SDL2_Mixer.Play_Channel (1, Audio.Waves (Sound), 0);
   end Play_Background_Sound;

   procedure Play_Sound
     (Audio : in out Audio_Driver;
      Sound : in     Sound_ID_Type)
   is
   begin
      SDL2_Mixer.Play_Channel (0, Audio.Waves (Sound), 0);
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

	 Chunk := SDL2_Mixer.Load_WAV (Filepath.Get (SB.To_String (Sound_Array (I).Name), "wavs"));
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
