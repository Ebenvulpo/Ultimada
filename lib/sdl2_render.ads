with SDL2_Rect;    use SDL2_Rect;
with SDL2_Stdinc;  use SDL2_Stdinc;
with SDL2_Surface; use SDL2_Surface;
with SDL2_Video;   use SDL2_Video;

package SDL2_Render is
   type Dummy_SDL_Renderer is limited private;
   type Dummy_SDL_Texture  is limited private;

   type SDL_Renderer is access Dummy_SDL_Renderer;
   type SDL_Texture  is access Dummy_SDL_Texture;
   pragma Convention (C, SDL_Renderer);
   pragma Convention (C, SDL_Texture);

   ---------------------------
   --  Color Channel Types  --
   ---------------------------
   type Red_Color_Channel_Type   is new Uint8;
   type Green_Color_Channel_Type is new Uint8;
   type Blue_Color_Channel_Type  is new Uint8;
   type Alpha_Color_Channel_Type is new Uint8;

   --------------------------
   --  Public Subprograms  --
   --------------------------

   --  Renderer Subprograms
   function  Create_Renderer  (Window   : in     SDL_Window) return SDL_Renderer;
   procedure Destroy_Renderer (Renderer : in out SDL_Renderer);

   procedure Render_Fill_Rectangle
     (Renderer  : in     SDL_Renderer;
      Rectangle : access SDL_Rectangle);

   procedure Set_Render_Draw_Color
     (Renderer : in SDL_Renderer;
      Red      : in Red_Color_Channel_Type;
      Green    : in Green_Color_Channel_Type;
      Blue     : in Blue_Color_Channel_Type;
      Alpha    : in Alpha_Color_Channel_Type);

   procedure Render_Clear (Renderer : in SDL_Renderer) with
     Import => True, Convention => C, External_Name => "SDL_RenderClear";

   procedure Render_Copy
     (Renderer : in     SDL_Renderer;
      Texture  : in     SDL_Texture;
      SrcRect  : access SDL_Rectangle;
      DstRect  : access SDL_Rectangle);

   procedure Render_Present (Renderer : in SDL_Renderer) with
     Import => True, Convention => C, External_Name => "SDL_RenderPresent";

   procedure Render_Set_Logical_Size
     (Renderer : in SDL_Renderer;
      Width    : in Integer;
      Height   : in Integer);

   --  Texture Subprograms
   function Create_Texture_From_Surface
     (Renderer : in SDL_Renderer;
      Surface  : in SDL_Surface)
     return SDL_Texture;

   procedure Destroy_Texture (Texture : in out SDL_Texture);

   ------------------
   --  Exceptions  --
   ------------------
   Renderer_Error : exception;

private
   type Dummy_SDL_Renderer is null record;
   type Dummy_SDL_Texture  is null record;
   pragma Convention (C, Dummy_SDL_Renderer);
   pragma Convention (C, Dummy_SDL_Texture);
end SDL2_Render;
