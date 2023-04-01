package body Player is
   procedure Create
     (Player : out Player_Type;
      Name   : in  String := "Ebenvulpo";
      X      : in  Location_X := 0;
      Y      : in  Location_Y := 0)
   is
   begin
      Player.Name (Name'First .. Name'Last) := Name;
   end Create;

   function Get_Name (Player : in out Player_Type) return String is
   begin
      return Player.Name;
   end Get_Name;
end Player;
