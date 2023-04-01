with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with SDL2;         use SDL2;

procedure Ultimada is
   package C renames Interfaces.C;

   Error    :         C.int;
   Event    : aliased SDL_Event;
   Window   :         SDL_Window;
   Renderer :         SDL_Renderer;
begin
   Error := SDL_Init (16#0000_0020# or 16#0000_4000# or 16#0000_0010#);
   if Error < 0 then
      raise Program_Error;
   end if;
   
   Window := SDL_CreateWindow ("Test", 200, 200, 640, 480, 0);
   Renderer := SDL_CreateRenderer (Window, -1, 0);

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
   end loop;
<<Exit_Loop>>

   Ada.Text_IO.Put_Line ("Hello World!");
   SDL_Quit;
end Ultimada;
