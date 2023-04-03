package body Player is
   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Initialize
     (Player : out Player_Type;
      Name   : in  String := "Ebenvulpo";
      X      : in  Location_X := 0;
      Y      : in  Location_Y := 0)
   is
   begin
      Player.Name (Name'First .. Name'Last) := Name;
      Player.X := X;
      Player.Y := Y;
   end Initialize;   

   --------------------------
   --  Player Subprograms  --
   --------------------------
   procedure Change_Location
     (Player : out Player_Type;
      X      : in  Location_X;
      Y      : in  Location_Y)
   is
   begin
      --  if 0 <= X and 0 <= Y then
      Player.X := X;
      Player.Y := Y;
      --  end if;
   end Change_Location;

   procedure Get_Location
     (Player : in  Player_Type;
      X      : out Location_X;
      Y      : out Location_Y)
   is
   begin
      X := Player.X;
      Y := Player.Y;
   end Get_Location;

   function Get_Name (Player : in out Player_Type) return String is
   begin
      return Player.Name;
   end Get_Name;
end Player;
