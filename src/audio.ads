with Ada.Strings.Bounded;
with SDL2;                use SDL2;

package Audio is
   type Audio_Driver is tagged private;

   type Sound_ID_Type is new Natural;

   -----------------
   --  Constants  --
   -----------------
   Cricket_Sound             : constant Sound_ID_Type := 0;
   Drop_Sound                : constant Sound_ID_Type := 1;
   Hegh_Sound                : constant Sound_ID_Type := 2;
   Hwom_Sound                : constant Sound_ID_Type := 3;
   Machine_Whine_Sound       : constant Sound_ID_Type := 4;
   Machine_Whirr_Sound       : constant Sound_ID_Type := 5;
   Machine_Whirr2_Sound      : constant Sound_ID_Type := 6;
   Machine_Whirr3_Purr_Sound : constant Sound_ID_Type := 7;
   Monster_Sound             : constant Sound_ID_Type := 8;
   Monster_Die_Sound         : constant Sound_ID_Type := 9;
   People_Background_Sound   : constant Sound_ID_Type := 10;
   Pop_Sound                 : constant Sound_ID_Type := 11;
   SSS_Sound                 : constant Sound_ID_Type := 12;
   Violin_AE_Pluck_Sound     : constant Sound_ID_Type := 13;
   Violin_E_Sound            : constant Sound_ID_Type := 14;
   Violin_G_Sound            : constant Sound_ID_Type := 15;
   Wind_Sound                : constant Sound_ID_Type := 16;
   Wind2_Sound               : constant Sound_ID_Type := 17;

   Sound_None                : constant Sound_ID_Type := 128;

   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize (Audio : in out Audio_Driver);
   procedure Initialize   (Audio : in out Audio_Driver);

   -------------------------
   --  Audio Subprograms  --
   -------------------------
   procedure Play_Background_Sound
     (Audio : in out Audio_Driver;
      Sound : in     Sound_ID_Type);

   procedure Play_Sound
     (Audio : in out Audio_Driver;
      Sound : in     Sound_ID_Type);

private
   type WAV_Array_Type is array (Sound_ID_Type range <>) of Mix_Chunk;

   type Audio_Driver is tagged
      record
	 Waves : Wav_Array_Type (0 .. 64) := (others => null);
      end record;

   procedure Load_Audio_Files (Audio : in out Audio_Driver);

   package SB is new Ada.Strings.Bounded.Generic_Bounded_Length (64);

   type Sound_Type is
      record
	 Name   : SB.Bounded_String := SB.To_Bounded_String ("None");
	 Number : Sound_ID_Type     := Sound_None;
      end record;

   type Sound_Array_Type is array (Sound_ID_Type range <>) of Sound_Type;
   Sound_Array : constant Sound_Array_Type :=
     (Sound_Type'(SB.To_Bounded_String ("cricket.wav"),             Cricket_Sound),
      Sound_Type'(SB.To_Bounded_String ("drop.wav"),                Drop_Sound),
      Sound_Type'(SB.To_Bounded_String ("hegh.wav"),                Hegh_Sound),
      Sound_Type'(SB.To_Bounded_String ("hwom.wav"),                Hwom_Sound),
      Sound_Type'(SB.To_Bounded_String ("machine_whine.wav"),       Machine_Whine_Sound),
      Sound_Type'(SB.To_Bounded_String ("machine_whirr.wav"),       Machine_Whirr_Sound),
      Sound_Type'(SB.To_Bounded_String ("machine_whirr2.wav"),      Machine_Whirr2_Sound),
      Sound_Type'(SB.To_Bounded_String ("machine_whirr3_purr.wav"), Machine_Whirr3_Purr_Sound),
      Sound_Type'(SB.To_Bounded_String ("monster.wav"),             Monster_Sound),
      Sound_Type'(SB.To_Bounded_String ("monster_die.wav"),         Monster_Die_Sound),
      Sound_Type'(SB.To_Bounded_String ("people_background.wav"),   People_Background_Sound),
      Sound_Type'(SB.To_Bounded_String ("pop.wav"),                 Pop_Sound),
      Sound_Type'(SB.To_Bounded_String ("sss.wav"),                 SSS_Sound),
      Sound_Type'(SB.To_Bounded_String ("violin_AE_pluck.wav"),     Violin_AE_Pluck_Sound),
      Sound_Type'(SB.To_Bounded_String ("violin_E.wav"),            Violin_E_Sound),
      Sound_Type'(SB.To_Bounded_String ("violin_G.wav"),            Violin_G_Sound),
      Sound_Type'(SB.To_Bounded_String ("wind.wav"),                Wind_Sound),
      Sound_Type'(SB.To_Bounded_String ("wind2.wav"),               Wind2_Sound));
end Audio;
