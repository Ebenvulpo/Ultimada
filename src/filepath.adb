with SDL2; use SDL2;

package body Filepath is
   ----------------
   --  Constants --
   ----------------
   Windows : constant String := "Windows";
   Linux   : constant String := "Linux";

   Assets  : constant String := "assets";

   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize is
   begin
      if
	App_Filepath  = C.Strings.Null_Ptr or
	Platform_Name = C.Strings.Null_Ptr
      then
	 raise Program_Error;
      end if;

      Free (Platform_Name);
      Free (App_Filepath);
   end Deinitialize;

   procedure Initialize is
   begin
      if
	App_Filepath  /= C.Strings.Null_Ptr or
	Platform_Name /= C.Strings.Null_Ptr
      then
	 raise Program_Error;
      end if;

      Platform_Name := SDL_GetPlatform;
      App_Filepath  := SDL_GetBasePath;
      if
	Platform_Name = C.Strings.Null_ptr or
	App_Filepath  = C.Strings.Null_Ptr
      then
	 raise Program_Error;
      end if;
   end Initialize;

   ---------------------------------
   --  Getting Files Subprograms  --
   ---------------------------------
   function Get_BMP (Name : in String) return String is
      B : constant String := "bmps";
   begin
      if Value (Platform_Name) = Windows then
	 return Value (App_Filepath) & Assets & "\" & B & "\" & Name;
      elsif Value (Platform_Name) = Linux then
	 return Value (App_Filepath) & Assets & "/" & B & "/" & Name;
      else --  Assuming another Unix style system.
	 return Value (App_Filepath) & Assets & "/" & B & "/" & Name;
      end if;
   end Get_BMP;

   function Get_WAV (Name : in String) return String is
      W : constant String := "wavs";
   begin
      if Value (Platform_Name) = Windows then
	 return Value (App_Filepath) & Assets & "\" & W & "\" & Name;
      elsif Value (Platform_Name) = Linux then
	 return Value (App_Filepath) & Assets & "/" & W & "/" & Name;
      else  --  Assuming another Unix style system.
	 return Value (App_Filepath) & Assets & "/" & W & "/" & Name;
      end if;
   end Get_WAV;
end Filepath;
