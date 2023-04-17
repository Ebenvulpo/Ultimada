with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Map;
with Person;

package Player is
   type Player_Type is tagged private;

   subtype Location_X is Map.Map_Width_Type;
   subtype Location_Y is Map.Map_Height_Type;

   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Initialize
     (Player : out Player_Type;
      Name   : in  String     := "Ebenvulpo";
      ID     : in  Person.Person_Type;
      X      : in  Location_X := 0;
      Y      : in  Location_Y := 0);

   --------------------------
   --  Player Subprograms  --
   --------------------------
   procedure Change_Location
     (Player : out Player_Type;
      X      : in  Location_X;
      Y      : in  Location_Y);

   function Get_ID (Player : in Player_Type) return Person.Person_Type;

   procedure Get_Location
     (Player : in  Player_Type;
      X      : out Location_X;
      Y      : out Location_Y);

   function Get_Name (Player : in out Player_Type) return String;

private
   type Player_Type is tagged
      record
	 Name : String (1 .. 16)   := (others => Nul);
	 ID   : Person.Person_Type := Person.Person_None;
	 X    : Location_X := 0;
	 Y    : Location_Y := 0;
      end record;
end Player;
