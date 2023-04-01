with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with SDL2;         use SDL2;
with System;       use System;

procedure Ultimada is
   package C renames Interfaces.C;

   Error    :         C.int;
   Event    : aliased SDL_Event;
   Window   :         SDL_Window;
   Renderer :         SDL_Renderer;
   Surface  :         SDL_Surface;
   Texture  :         SDL_Texture;
begin
   Error := SDL_Init (16#0000_0020# or 16#0000_4000# or 16#0000_0010#);
   if Error < 0 then
      raise Program_Error;
   end if;

   Ada.Text_IO.Put_Line ("Hello World Ultimada!");

   Window := SDL_CreateWindow ("Test", 200, 200, 640, 480, 0);
   Renderer := SDL_CreateRenderer (Window, -1, 0);
   
   --  The following code doest not work...

   --  Surface := SDL_LoadBMP ("test.bmp");
   --  if Surface = SDL_Surface (System.Null_Address) then
   --     raise Program_Error;
   --  end if;

   --  Texture := SDL_CreateTextureFromSurface (Renderer, Surface);
   --  if Texture = SDL_Texture (System.Null_Address) then
   --     raise Program_Error;
   --  end if;

   --  SDL_FreeSurface (Surface);

   loop
      Error := SDL_WaitEvent (Event'Access);
      case Event.Event_Type is
	 when 16#100# => goto Exit_Loop;
	 when others  => null;
      end case;

      Error := SDL_SetRenderDrawColor (Renderer, 16#00#, 16#00#, 16#00#, 16#FF#);
      if Error < 0 then
	 raise Program_Error;
      end if;

      SDL_RenderClear (Renderer);
      --  Error := SDL_RenderCopy (Renderer, Texture, null, null);
      SDL_RenderPresent (Renderer);
   end loop;
<<Exit_Loop>>

   SDL_DestroyRenderer (Renderer);
   SDL_DestroyWindow (Window);

   SDL_Quit;
end Ultimada;
