with Ada.Text_IO;
with Filepath;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Audio is
   package C renames Interfaces.C;

   procedure Init (Audio : in out Audio_Driver) is
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
   end Init;

   procedure Finalize (Audio : in out Audio_Driver) is
   begin
      for I in Audio.Waves'Range loop
	 if Audio.Waves (I) /= null then
	    Mix_FreeChunk (Audio.Waves (I));
	 end if;
      end loop;

      Mix_Quit;
   end Finalize;

   procedure Load_Audio_Files (Audio : in out Audio_Driver) is
      Chunk : Mix_Chunk;
   begin
      Ada.Text_IO.Put_Line ("Loading Audio Files...");
      for I in Wav_Files_Array'Range loop
	 declare
	    File : aliased constant C.char_array := C.To_C (Value (Filepath.Get) & "assets\" & "wavs\" & SB.To_String (Wav_Files_Array (I)));
	 begin
	    Ada.Text_IO.Put      ("Loading: ");
	    Ada.Text_IO.Put_Line (SB.To_String (Wav_Files_Array (I)));

	    Chunk := Mix_LoadWAV (File'Address);
	    if Chunk = null then
	       raise Program_Error;
	    end if;

	    Audio.Waves (I) := Chunk;
	 end;
      end loop;
   end Load_Audio_Files;
end Audio;
