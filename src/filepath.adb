with SDL2; use SDL2;

package body Filepath is
   ----------------
   --  Constants --
   ----------------
   Windows : constant String := "Windows";

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
   function Get
     (Name : in String;
      T    : in String)
     return String
   is
      Slash : String := "/";
   begin
      if Value (Platform_Name) = Windows then
         Slash := "\";
      end if;
      return Value (App_Filepath) & Assets & Slash & T & Slash & Name;
   end Get;
end Filepath;
