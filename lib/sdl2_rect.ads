with SDL2_Stdinc;  use SDL2_Stdinc;

package SDL2_Rect is
   pragma Pure;

   type SDL_Rectangle is record
      X, Y : aliased Sint32;
      W, H : aliased Sint32;
   end record;
   pragma Convention (C, SDL_Rectangle);
end SDL2_Rect;
