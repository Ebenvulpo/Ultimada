with Ada.Strings.Bounded;
with SDL2;                use SDL2;

package Audio is
   type Audio_Driver is tagged private;

   procedure Deinit (Audio : in out Audio_Driver);
   procedure Init   (Audio : in out Audio_Driver);

   procedure Play_Sound
     (Audio : in out Audio_Driver;
      Sound : in     Natural);

private
   package SB is new Ada.Strings.Bounded.Generic_Bounded_Length (64);

   type Wav_Array_Type is array (Natural range <>) of Mix_Chunk;

   type Audio_Driver is tagged
      record
	 Waves : Wav_Array_Type (0 .. 64) := (others => null);
      end record;

   procedure Load_Audio_Files (Audio : in out Audio_Driver);

   type Wav_Files_Array_Type is array (Natural range <>) of SB.Bounded_String;
   Wav_Files_Array : constant Wav_Files_Array_Type := 
     (SB.To_Bounded_String ("cricket.wav"),           SB.To_Bounded_String ("drop.wav"),
      SB.To_Bounded_String ("hegh.wav"),              SB.To_Bounded_String ("hwom.wav"),
      SB.To_Bounded_String ("machine_whine.wav"),     SB.To_Bounded_String ("machine_whirr.wav"),
      SB.To_Bounded_String ("machine_whirr2.wav"),    SB.To_Bounded_String ("machine_whirr3_purr.wav"),
      SB.To_Bounded_String ("monster.wav"),           SB.To_Bounded_String ("monster_die.wav"),
      SB.To_Bounded_String ("people_background.wav"), SB.To_Bounded_String ("pop.wav"),
      SB.To_Bounded_String ("sss.wav"),               SB.To_Bounded_String ("violin_AE_pluck.wav"),
      SB.To_Bounded_String ("violin_E.wav"),          SB.To_Bounded_String ("violin_G.wav"),
      SB.To_Bounded_String ("wind.wav"),              SB.To_Bounded_String ("wind2.wav"));
end Audio;
