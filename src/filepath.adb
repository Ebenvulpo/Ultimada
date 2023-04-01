with SDL2; use SDL2;

package body Filepath is
   procedure Load_Filepath is
   begin
      if App_Filepath /= C.Strings.Null_Ptr then
	 raise Program_Error;
      end if;

      App_Filepath := SDL_GetBasePath;
   end Load_Filepath;
   
   procedure Free_Filepath is
   begin
      if App_Filepath = C.Strings.Null_Ptr then
	 raise Program_Error;
      end if;

      Free (App_Filepath);
   end Free_Filepath;

   function Get return chars_ptr is
   begin
      return App_Filepath;
   end Get;
end Filepath;
