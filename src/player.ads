with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Interfaces.C;
with Map;

package Player is
   package C renames Interfaces.C;

   type Player_Type is tagged private;

   subtype Location_X is Map.Map_Width_Type;
   subtype Location_Y is Map.Map_Height_Type;

   procedure Create
     (Player : out Player_Type;
      Name   : in  String     := "Ebenvulpo";
      X      : in  Location_X := 0;
      Y      : in  Location_Y := 0);

   function Get_Name (Player : in out Player_Type) return String;

private
   type Player_Type is tagged
      record
	 Name : String (1 .. 16) := (others => Nul);
	 X    : Location_X := 0;
	 Y    : Location_Y := 0;
      end record;
end Player;
