with Interfaces.C; use Interfaces.C;

package body SDL2_Render is
   package C renames Interfaces.C;

   ----------------------------
   --  External C Functions  --
   ----------------------------
   function C_SDL_CreateRenderer
     (Window : in SDL_Window;
      Index  : in C.int;
      Flags  : in C.unsigned)
     return SDL_Renderer with
     Import => True, Convention => C, External_Name => "SDL_CreateRenderer";

   procedure C_SDL_DestroyRenderer (Renderer : in SDL_Renderer) with
     Import => True, Convention => C, External_Name => "SDL_DestroyRenderer";

   function C_SDL_RenderFillRect
     (Renderer : in     SDL_Renderer;
      Rect     : access SDL_Rectangle)
     return C.int with
     Import => True, Convention => C, External_Name => "SDL_RenderFillRect";

   function C_SDL_SetRenderDrawColor
     (Renderer : in SDL_Renderer;
      R        : in Red_Color_Channel_Type;
      G        : in Green_Color_Channel_Type;
      B        : in Blue_Color_Channel_Type;
      A        : in Alpha_Color_Channel_Type)
     return C.int with
     Import => True, Convention => C, External_Name => "SDL_SetRenderDrawColor";

   function C_SDL_RenderCopy
     (Renderer : in     SDL_Renderer;
      Texture  : in     SDL_Texture;
      SrcRect  : access SDL_Rectangle;
      DstRect  : access SDL_Rectangle)
     return C.int with
     Import => True, Convention => C, External_Name => "SDL_RenderCopy";

   function C_SDL_RenderSetLogicalSize
     (Renderer : in SDL_Renderer;
      W        : in C.int;
      H        : in C.int)
     return C.int with
     Import => True, Convention => C, External_Name => "SDL_RenderSetLogicalSize";

   function C_SDL_CreateTextureFromSurface
     (Renderer : in SDL_Renderer;
      Surface  : in SDL_Surface)
     return SDL_Texture with
     Import => True, Convention => C, External_Name => "SDL_CreateTextureFromSurface";

   procedure C_SDL_DestroyTexture (Texture : in SDL_Texture) with
     Import => True, Convention => C, External_Name => "SDL_DestroyTexture";

   --------------------------
   --  Public Subprograms  --
   --------------------------

   --  Renderer Subprograms
   function Create_Renderer (Window : in SDL_Window) return SDL_Renderer is
      Renderer : SDL_Renderer;
   begin
      Renderer := C_SDL_CreateRenderer (Window, -1, 0);
      if Renderer = null then
	 raise Renderer_Error;
      end if;

      return Renderer;
   end Create_Renderer;

   procedure Destroy_Renderer (Renderer : in out SDL_Renderer) is
   begin
      C_SDL_DestroyRenderer (Renderer);
      Renderer := null;
   end Destroy_Renderer;

   procedure Render_Fill_Rectangle
     (Renderer  : in     SDL_Renderer;
      Rectangle : access SDL_Rectangle)
   is
      Error : C.int;
   begin
      Error := C_SDL_RenderFillRect (Renderer, Rectangle);
      if Error < 0 then
	 raise Renderer_Error;
      end if;
   end Render_Fill_Rectangle;

   procedure Set_Render_Draw_Color
     (Renderer : in SDL_Renderer;
      Red      : in Red_Color_Channel_Type;
      Green    : in Green_Color_Channel_Type;
      Blue     : in Blue_Color_Channel_Type;
      Alpha    : in Alpha_Color_Channel_Type)
   is
      Error : C.int;
   begin
      Error := C_SDL_SetRenderDrawColor (Renderer, Red, Green, Blue, Alpha);
      if Error < 0 then
	 raise Renderer_Error;
      end if;
   end Set_Render_Draw_Color;

   procedure Render_Copy
     (Renderer : in     SDL_Renderer;
      Texture  : in     SDL_Texture;
      SrcRect  : access SDL_Rectangle;
      DstRect  : access SDL_Rectangle)
   is
      Error : C.int;
   begin
      Error := C_SDL_RenderCopy (Renderer, Texture, SrcRect, DstRect);
      if Error < 0 then
	 raise Renderer_Error;
      end if;
   end Render_Copy;

   procedure Render_Set_Logical_Size
     (Renderer : in SDL_Renderer;
      Width    : in Integer;
      Height   : in Integer)
   is
      Error : C.int;
   begin
      Error := C_SDL_RenderSetLogicalSize
	(Renderer => Renderer,
	 W        => C.int (Width),
	 H        => C.int (Height));
      if Error < 0 then
	 raise Renderer_Error;
      end if;
   end Render_Set_Logical_Size;

   --  Texture Subprograms
   function Create_Texture_From_Surface
     (Renderer : in SDL_Renderer;
      Surface  : in SDL_Surface)
     return SDL_Texture
   is
      Texture : SDL_Texture;
   begin
      Texture := C_SDL_CreateTextureFromSurface (Renderer, Surface);
      if Texture = null then
	 raise Renderer_Error;
      end if;

      return Texture;
   end Create_Texture_From_Surface;

   procedure Destroy_Texture (Texture : in out SDL_Texture) is
   begin
      C_SDL_DestroyTexture (Texture);
      Texture := null;
   end Destroy_Texture;
end SDL2_Render;
