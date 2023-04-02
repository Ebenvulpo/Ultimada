with Ada.Finalization;
with SDL2;             use SDL2;

package Audio is
   type Audio_Driver is new Ada.Finalization.Controlled with private;

   procedure Init (Audio : in out Audio_Driver);

private
   type Audio_Driver is new Ada.Finalization.Controlled with
      record
	 null;
      end record;

   overriding procedure Finalize (Audio : in out Audio_Driver);
end Audio;
